#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <vector>
#include <streambuf>
#include "zlib.h"
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

/*
  Quick dump: fast C++ implementation of dump. Not as fully featured as the
  Java version. Reads the .hic file, finds the appropriate matrix and slice
  of data, and outputs as text in sparse upper triangular format.
  Currently only supporting matrices.
  Usage: juicebox-quick-dump <NONE/VC/VC_SQRT/KR> <hicFile(s)> <chr1>[:x1:x2] <chr2>[:y1:y2] <BP/FRAG> <binsize>
 */
// pointer structure for reading blocks or matrices, holds the size and position
struct indexEntry {
  int size;
  long position;
};

// sparse matrix entry
struct contactRecord {
  int binX;
  int binY;
  float counts;
};

// this is for creating a stream from a byte array for ease of use
struct membuf : std::streambuf
{
  membuf(char* begin, char* end) {
    this->setg(begin, begin, end);
  }
};

// version number
int version;

// map of block numbers to pointers
map <int, indexEntry> blockMap;

// returns whether or not this is valid HiC file
bool readMagicString(ifstream& fin) {
  string str;
  getline(fin, str, '\0' );
  return str[0]=='H' && str[1]=='I' && str[2]=='C';
}

// reads the header, storing the positions of the normalization vectors and returning the master pointer
long readHeader(ifstream& fin, string chr1, string chr2, int &c1pos1, int &c1pos2, int &c2pos1, int &c2pos2, int &chr1ind, int &chr2ind) {
  if (!readMagicString(fin)) {
    stop("Hi-C magic string is missing, does not appear to be a hic file\n");
    // exit(1);
  }

  fin.read((char*)&version, sizeof(int));
  if (version < 6) {
    stop("Version no longer supported\n");
    // exit(1);
  }
  long master;
  fin.read((char*)&master, sizeof(long));
  string genome;
  getline(fin, genome, '\0' );
  int nattributes;
  fin.read((char*)&nattributes, sizeof(int));
  // reading and ignoring attribute-value dictionary
  for (int i=0; i<nattributes; i++) {
    string key, value;
    getline(fin, key, '\0');
    getline(fin, value, '\0');
  }
  int nChrs;
  fin.read((char*)&nChrs, sizeof(int));
  // chromosome map for finding matrix
  bool found1 = false;
  bool found2 = false;
  for (int i=0; i<nChrs; i++) {
    string name;
    int length;
    getline(fin, name, '\0');
    fin.read((char*)&length, sizeof(int));
    if (name==chr1) {
      found1=true;
      chr1ind = i;
      if (c1pos1 == -100) {
	c1pos1 = 0;
	c1pos2 = length;
      }
    }
    if (name==chr2) {
      found2=true;
      chr2ind = i;
      if (c2pos1 == -100) {
	c2pos1 = 0;
	c2pos2 = length;
      }
    }
  }
  if (!found1 || !found2) {
    stop("One of the chromosomes wasn't found in the file. Check that the chromosome name matches the genome.\n");
    // exit(1);
  }
  return master;
}

// reads the footer from the master pointer location. takes in the chromosomes, norm, u (BP or FRAG) and resolution or
// binsize, and sets the file position of the matrix and the normalization vectors for those chromosomes at the given
// normalization and resolution
void readFooter(ifstream& fin, long master, int c1, int c2, string norm, string u, int resolution, long &myFilePos, indexEntry &c1NormEntry, indexEntry &c2NormEntry) {
  fin.seekg(master, ios::beg);
  int nBytes;
  fin.read((char*)&nBytes, sizeof(int));

  stringstream ss;
  ss << c1 << "_" << c2;
  string key = ss.str();

  int nEntries;
  fin.read((char*)&nEntries, sizeof(int));
  bool found = false;
  for (int i=0; i<nEntries; i++) {
    string str;
    getline(fin, str, '\0');
    long fpos;
    fin.read((char*)&fpos, sizeof(long));
    int sizeinbytes;
    fin.read((char*)&sizeinbytes, sizeof(int));
    if (str == key) {
      myFilePos = fpos;
      found=true;
    }
  }
  if (!found) {
    stop("File doesn't have the given chr_chr map\n");
    // exit(1);
  }

  if (norm=="NONE") return; // no need to read norm vector index

  // read in and ignore expected value maps; don't store; reading these to
  // get to norm vector index
  int nExpectedValues;
  fin.read((char*)&nExpectedValues, sizeof(int));
  for (int i=0; i<nExpectedValues; i++) {
    string str;
    getline(fin, str, '\0'); //u
    int binSize;
    fin.read((char*)&binSize, sizeof(int));

    int nValues;
    fin.read((char*)&nValues, sizeof(int));
    for (int j=0; j<nValues; j++) {
      double v;
      fin.read((char*)&v, sizeof(double));
    }

    int nNormalizationFactors;
    fin.read((char*)&nNormalizationFactors, sizeof(int));
    for (int j=0; j<nNormalizationFactors; j++) {
      int chrIdx;
      fin.read((char*)&chrIdx, sizeof(int));
      double v;
      fin.read((char*)&v, sizeof(double));
    }
  }
  fin.read((char*)&nExpectedValues, sizeof(int));
  for (int i=0; i<nExpectedValues; i++) {
    string str;
    getline(fin, str, '\0'); //typeString
    getline(fin, str, '\0'); //u
    int binSize;
    fin.read((char*)&binSize, sizeof(int));

    int nValues;
    fin.read((char*)&nValues, sizeof(int));
    for (int j=0; j<nValues; j++) {
      double v;
      fin.read((char*)&v, sizeof(double));
    }
    int nNormalizationFactors;
    fin.read((char*)&nNormalizationFactors, sizeof(int));
    for (int j=0; j<nNormalizationFactors; j++) {
      int chrIdx;
      fin.read((char*)&chrIdx, sizeof(int));
      double v;
      fin.read((char*)&v, sizeof(double));
    }
  }
  // Index of normalization vectors
  fin.read((char*)&nEntries, sizeof(int));
  bool found1 = false;
  bool found2 = false;
  for (int i = 0; i < nEntries; i++) {
    string normtype;
    getline(fin, normtype, '\0'); //normalization type
    int chrIdx;
    fin.read((char*)&chrIdx, sizeof(int));
    string unit1;
    getline(fin, unit1, '\0'); //unit
    int resolution1;
    fin.read((char*)&resolution1, sizeof(int));
    long filePosition;
    fin.read((char*)&filePosition, sizeof(long));
    int sizeInBytes;
    fin.read((char*)&sizeInBytes, sizeof(int));
    if (chrIdx == c1 && normtype == norm && unit1 == u && resolution1 == resolution) {
      c1NormEntry.position=filePosition;
      c1NormEntry.size=sizeInBytes;
      found1 = true;
    }
    if (chrIdx == c2 && normtype == norm && unit1 == u && resolution1 == resolution) {
      c2NormEntry.position=filePosition;
      c2NormEntry.size=sizeInBytes;
      found2 = true;
    }
  }
  if (!found1 || !found2) {
    stop("File did not contain normalization factors for one or both chromosomes\n");
    // exit(1);
  }
}

// reads the raw binned contact matrix at specified resolution, setting the block bin count and block column count
bool readMatrixZoomData(ifstream& fin, string myunit, int mybinsize, int &myBlockBinCount, int &myBlockColumnCount) {
  string u;
  getline(fin, u, '\0' ); // unit
  int tmp;
  fin.read((char*)&tmp, sizeof(int)); // Old "zoom" index -- not used
  float tmp2;
  fin.read((char*)&tmp2, sizeof(float)); // sumCounts
  fin.read((char*)&tmp2, sizeof(float)); // occupiedCellCount
  fin.read((char*)&tmp2, sizeof(float)); // stdDev
  fin.read((char*)&tmp2, sizeof(float)); // percent95
  int binSize;
  fin.read((char*)&binSize, sizeof(int));
  int blockBinCount;
  fin.read((char*)&blockBinCount, sizeof(int));
  int blockColumnCount;
  fin.read((char*)&blockColumnCount, sizeof(int));

  bool storeBlockData = false;
  if (myunit==u && mybinsize==binSize) {
    myBlockBinCount = blockBinCount;
    myBlockColumnCount = blockColumnCount;
    storeBlockData = true;
  }

  int nBlocks;
  fin.read((char*)&nBlocks, sizeof(int));

  for (int b = 0; b < nBlocks; b++) {
    int blockNumber;
    fin.read((char*)&blockNumber, sizeof(int));
    long filePosition;
    fin.read((char*)&filePosition, sizeof(long));
    int blockSizeInBytes;
    fin.read((char*)&blockSizeInBytes, sizeof(int));
    indexEntry entry;
    entry.size = blockSizeInBytes;
    entry.position = filePosition;
    if (storeBlockData) blockMap[blockNumber] = entry;
  }
  return storeBlockData;
}

// goes to the specified file pointer and finds the raw contact matrix at specified resolution, calling readMatrixZoomData.
// sets blockbincount and blockcolumncount
void readMatrix(ifstream& fin, long myFilePosition, string u, int resolution, int &myBlockBinCount, int &myBlockColumnCount) {
  fin.seekg(myFilePosition, ios::beg);
  int c1,c2;
  fin.read((char*)&c1, sizeof(int)); //chr1
  fin.read((char*)&c2, sizeof(int)); //chr2
  int nRes;
  fin.read((char*)&nRes, sizeof(int));
  int i=0;
  bool found=false;
  while (i<nRes && !found) {
    found = readMatrixZoomData(fin, u, resolution, myBlockBinCount, myBlockColumnCount);
    i++;
  }
  if (!found) {
    stop("Error finding block data\n");
    // exit(1);
  }
}
// gets the blocks that need to be read for this slice of the data.  needs blockbincount, blockcolumncount, and whether
// or not this is intrachromosomal.
set<int> getBlockNumbersForRegionFromBinPosition(int* regionIndices, int blockBinCount, int blockColumnCount, bool intra) {
   int col1 = regionIndices[0] / blockBinCount;
   int col2 = (regionIndices[1] + 1) / blockBinCount;
   int row1 = regionIndices[2] / blockBinCount;
   int row2 = (regionIndices[3] + 1) / blockBinCount;

   set<int> blocksSet;
   // first check the upper triangular matrix
   for (int r = row1; r <= row2; r++) {
     for (int c = col1; c <= col2; c++) {
       int blockNumber = r * blockColumnCount + c;
       blocksSet.insert(blockNumber);
     }
   }
   // check region part that overlaps with lower left triangle
   // but only if intrachromosomal
   if (intra) {
     for (int r = col1; r <= col2; r++) {
       for (int c = row1; c <= row2; c++) {
	 int blockNumber = r * blockColumnCount + c;
	 blocksSet.insert(blockNumber);
       }
     }
   }

   return blocksSet;
}

// this is the meat of reading the data.  takes in the block number and returns the set of contact records corresponding to
// that block.  the block data is compressed and must be decompressed using the zlib library functions
vector<contactRecord> readBlock(ifstream& fin, int blockNumber) {
  indexEntry idx = blockMap[blockNumber];
  if (idx.size == 0) {
    vector<contactRecord> v;
    return v;
  }
  char compressedBytes[idx.size];
  char* uncompressedBytes = new char[idx.size*10]; //biggest seen so far is 3
  fin.seekg(idx.position, ios::beg);
  fin.read(compressedBytes, idx.size);
  // Decompress the block
  // zlib struct
  z_stream infstream;
  infstream.zalloc = Z_NULL;
  infstream.zfree = Z_NULL;
  infstream.opaque = Z_NULL;
  infstream.avail_in = (uInt)(idx.size); // size of input
  infstream.next_in = (Bytef *)compressedBytes; // input char array
  infstream.avail_out = (uInt)idx.size*10; // size of output
  infstream.next_out = (Bytef *)uncompressedBytes; // output char array
  // the actual decompression work.
  inflateInit(&infstream);
  inflate(&infstream, Z_NO_FLUSH);
  inflateEnd(&infstream);
  int uncompressedSize=infstream.total_out;

  // create stream from buffer for ease of use
  membuf sbuf(uncompressedBytes, uncompressedBytes + uncompressedSize);
  istream bufferin(&sbuf);
  int nRecords;
  bufferin.read((char*)&nRecords, sizeof(int));
  vector<contactRecord> v(nRecords);
  // different versions have different specific formats
  if (version < 7) {
    for (int i = 0; i < nRecords; i++) {
      int binX, binY;
      bufferin.read((char*)&binX, sizeof(int));
      bufferin.read((char*)&binY, sizeof(int));
      float counts;
      bufferin.read((char*)&counts, sizeof(float));
      contactRecord record;
      record.binX = binX;
      record.binY = binY;
      record.counts = counts;
      v[i] = record;
    }
  }
  else {
    int binXOffset, binYOffset;
    bufferin.read((char*)&binXOffset, sizeof(int));
    bufferin.read((char*)&binYOffset, sizeof(int));
    char useShort;
    bufferin.read((char*)&useShort, sizeof(char));
    char type;
    bufferin.read((char*)&type, sizeof(char));
    int index=0;
    if (type == 1) {
      // List-of-rows representation
      short rowCount;
      bufferin.read((char*)&rowCount, sizeof(short));
      for (int i = 0; i < rowCount; i++) {
	short y;
	bufferin.read((char*)&y, sizeof(short));
	int binY = y + binYOffset;
	short colCount;
	bufferin.read((char*)&colCount, sizeof(short));
	for (int j = 0; j < colCount; j++) {
	  short x;
	  bufferin.read((char*)&x, sizeof(short));
	  int binX = binXOffset + x;
	  float counts;
	  if (useShort == 0) { // yes this is opposite of usual
	    short c;
	    bufferin.read((char*)&c, sizeof(short));
	    counts = c;
	  }
	  else {
	    bufferin.read((char*)&counts, sizeof(float));
	  }
	  contactRecord record;
	  record.binX = binX;
	  record.binY = binY;
	  record.counts = counts;
	  v[index]=record;
	  index++;
	}
      }
    }
    else if (type == 2) { // have yet to find test file where this is true, possibly entirely deprecated
      int nPts;
      bufferin.read((char*)&nPts, sizeof(int));
      short w;
      bufferin.read((char*)&w, sizeof(short));

      for (int i = 0; i < nPts; i++) {
	//int idx = (p.y - binOffset2) * w + (p.x - binOffset1);
	int row = i / w;
	int col = i - row * w;
	int bin1 = binXOffset + col;
	int bin2 = binYOffset + row;

	float counts;
	if (useShort == 0) { // yes this is opposite of the usual
	  short c;
	  bufferin.read((char*)&c, sizeof(short));
	  if (c != -32768) {
	    contactRecord record;
	    record.binX = bin1;
	    record.binY = bin2;
	    record.counts = c;
	    v[index]=record;
	    index++;
	  }
	}
	else {
	  bufferin.read((char*)&counts, sizeof(float));
	  if (counts != 0x7fc00000) { // not sure this works
	    //	  if (!Float.isNaN(counts)) {
	    contactRecord record;
	    record.binX = bin1;
	    record.binY = bin2;
	    record.counts = counts;
	    v[index]=record;
	    index++;
	  }
	}
      }
    }
  }
  delete[] uncompressedBytes; // don't forget to delete your heap arrays in C++!
  return v;
}

// reads the normalization vector from the file at the specified location
vector<double> readNormalizationVector(ifstream& fin, indexEntry entry) {
  char buffer[entry.size];
  fin.seekg(entry.position, ios::beg);
  fin.read(buffer, entry.size);
  membuf sbuf(buffer, buffer + entry.size);
  istream bufferin(&sbuf);
  int nValues;
  bufferin.read((char*)&nValues, sizeof(int));
  vector<double> values(nValues);
  //  bool allNaN = true;

  for (int i = 0; i < nValues; i++) {
    double d;
    bufferin.read((char*)&d, sizeof(double));
    values[i] = d;
    /* if (!Double.isNaN(values[i])) {
      allNaN = false;
      }*/
  }
  //  if (allNaN) return null;
  return values;
}

//' straw
//'
//' Adapted C++ implementation of Juicer's dump. Reads the .hic file, finds the
//' appropriate matrix and slice of data, and outputs as an R DataFrame.
//' 
//' Usage: straw <NONE/VC/VC_SQRT/KR> <hicFile(s)> <chr1>[:x1:x2] <chr2>[:y1:y2] <BP/FRAG> <binsize>
//' 
//' @importFrom Rcpp sourceCpp
//' @param norm Normalization to apply. Must be one of NONE/VC/VC_SQRT/KR.
//'     VC is vanilla coverage, VC_SQRT is square root of vanilla coverage,
//'     and KR is Knight-Ruiz or Balanced normalization.
//' @param fn path to the .hic file
//' @param ch1 first chromosome location (e.g., "1")
//' @param ch2 second chromosome location (e.g., "8")
//' @param u BP (BasePair) or FRAG (restriction enzyme FRAGment)
//' @param bs The bin size. By default, for BP, this is one of 
//'     <2500000, 1000000, 500000,
//'     250000, 100000, 50000, 25000, 10000, 5000> and for FRAG this is one of 
//'     <500, 200,
//'     100, 50, 20, 5, 2, 1>.
//' @return Data.frame of a sparse matrix of data from hic file. x,y,counts
// [[Rcpp::export]]
Rcpp::DataFrame straw(std::string norm, std::string fn, std::string ch1, std::string ch2, std::string u, int bs)
{
  blockMap.clear();
  if (!(u=="BP"||u=="FRAG")) {
    stop("Norm specified incorrectly, must be one of <BP/FRAG>\nUsage: juicebox-quick-dump <NONE/VC/VC_SQRT/KR> <hicFile(s)> <chr1>[:x1:x2] <chr2>[:y1:y2] <BP/FRAG> <binsize>\n");
  }

  ifstream fin(fn, fstream::in);
  if (!fin) {
    stop("File cannot be opened for reading\n");
  }
  stringstream ss(ch1);
  string chr1, chr2, x, y;
  int c1pos1=-100, c1pos2=-100, c2pos1=-100, c2pos2=-100;
  getline(ss, chr1, ':');
  if (getline(ss, x, ':') && getline(ss, y, ':')) {
    c1pos1 = stoi(x);
    c1pos2 = stoi(y);
  }
  stringstream ss1(ch2);
  getline(ss1, chr2, ':');
  if (getline(ss1, x, ':') && getline(ss1, y, ':')) {
    c2pos1 = stoi(x);
    c2pos2 = stoi(y);
  }
  int chr1ind, chr2ind;
  long master = readHeader(fin, chr1, chr2, c1pos1, c1pos2, c2pos1, c2pos2, chr1ind, chr2ind);

  int c1=min(chr1ind,chr2ind);
  int c2=max(chr1ind,chr2ind);
  int origRegionIndices[4]; // as given by user
  int regionIndices[4]; // used to find the blocks we need to access
  // reverse order if necessary
  if (chr1ind > chr2ind) {
    origRegionIndices[0] = c2pos1;
    origRegionIndices[1] = c2pos2;
    origRegionIndices[2] = c1pos1;
    origRegionIndices[3] = c1pos2;
    regionIndices[0] = c2pos1 / bs;
    regionIndices[1] = c2pos2 / bs;
    regionIndices[2] = c1pos1 / bs;
    regionIndices[3] = c1pos2 / bs;
  }
  else {
    origRegionIndices[0] = c1pos1;
    origRegionIndices[1] = c1pos2;
    origRegionIndices[2] = c2pos1;
    origRegionIndices[3] = c2pos2;
    regionIndices[0] = c1pos1 / bs;
    regionIndices[1] = c1pos2 / bs;
    regionIndices[2] = c2pos1 / bs;
    regionIndices[3] = c2pos2 / bs;
  }

  indexEntry c1NormEntry, c2NormEntry;
  long myFilePos;

  // readFooter will assign the above variables
  readFooter(fin, master, c1, c2, norm, u, bs, myFilePos, c1NormEntry, c2NormEntry);

  vector<double> c1Norm;
  vector<double> c2Norm;

  if (norm != "NONE") {
    c1Norm = readNormalizationVector(fin, c1NormEntry);
    c2Norm = readNormalizationVector(fin, c2NormEntry);
  }
  int blockBinCount, blockColumnCount;
  // readMatrix will assign blockBinCount and blockColumnCount
  readMatrix(fin, myFilePos, u, bs, blockBinCount, blockColumnCount);

  set<int> blockNumbers = getBlockNumbersForRegionFromBinPosition(regionIndices, blockBinCount, blockColumnCount, c1==c2);

  // getBlockIndices
  vector<contactRecord> records;
  vector<int> xActual_vec, yActual_vec;
  vector<float> counts_vec;
  for (set<int>::iterator it=blockNumbers.begin(); it!=blockNumbers.end(); ++it) {
    // get contacts in this block
    records = readBlock(fin, *it);
    for (vector<contactRecord>::iterator it2=records.begin(); it2!=records.end(); ++it2) {
      contactRecord rec = *it2;

      int xActual = rec.binX * bs;
      int yActual = rec.binY * bs;
      float counts = rec.counts;
      if (norm != "NONE") {
	counts = counts / (c1Norm[rec.binX] * c2Norm[rec.binY]);
      }
//      cout << xActual << " " << yActual << " " << counts << endl;
      if ((xActual >= origRegionIndices[0] && xActual <= origRegionIndices[1] &&
	   yActual >= origRegionIndices[2] && yActual <= origRegionIndices[3]) ||
	  // or check regions that overlap with lower left
	  ((c1==c2) && yActual >= origRegionIndices[0] && yActual <= origRegionIndices[1] && xActual >= origRegionIndices[2] && xActual <= origRegionIndices[3])) {
	  //printf("%d\t%d\t%.14g\n", xActual, yActual, counts);
    xActual_vec.push_back(xActual);
    yActual_vec.push_back(yActual);
    counts_vec.push_back(counts);
      }
    }
  }
  return Rcpp::DataFrame::create(Rcpp::Named("x") = xActual_vec, Rcpp::Named("y") = yActual_vec, Rcpp::Named("counts") = counts_vec);
}

///*
//SSE Instructions
#include <emmintrin.h>
void dgemm( int m, int n, float *A, float *C )
{
  int i;
  for(i = 0; i < (m/4)*4; i+=4){
    for(int k = 0; k < n; k++){
      for(int j = 0; j < m; j++){
        __m128 c_col = _mm_loadu_ps(&C[i+j*m]);
        __m128 ai_col = _mm_loadu_ps(&A[i+k*m]);
        __m128 aj_val = _mm_set1_ps(A[j+k*m]);

        __m128 mul = _mm_mul_ps(ai_col, aj_val);
        c_col = _mm_add_ps(c_col, mul);
        _mm_storeu_ps(&C[i+j*m], c_col);
	  }
	}
  }
  for(; i < m; i++)
    for(int k = 0; k < n; k++) 
      for(int j = 0; j < m; j++) 
        C[i+j*m] += A[i+k*m] * A[j+k*m];
}
//*/

/*
//Cache Locality Matrix Blocking
void dgemm( int m, int n, float *A, float *C )
{
  int bigk;
  for(bigk = 0; bigk<(n/4)*4; bigk+=4){
  	int bigj;
    for(bigj = 0; bigj<(m/4)*4; bigj+=4){
      for(int i = 0; i < m; i++)
        for(int k = bigk; k < bigk + 4; k++) 
          for(int j = bigj; j < bigj + 4; j++) 
            C[i+j*m] += A[i+k*m] * A[j+k*m];
    }
    for(int i = 0; i < m; i++)
      for(int k = bigk; k < bigk + 4; k++)       	
        for(int j = bigj; j < m; j++) 
          C[i+j*m] += A[i+k*m] * A[j+k*m];
  }
  for(int i = 0; i < m; i++)
    for(int k = (n/4)*4; k < n; k++) 
      for(int j = 0; j < m; j++) 
        C[i+j*m] += A[i+k*m] * A[j+k*m];
}
*/

/*
//Register Blocking (Ordered for saving)
void dgemm( int m, int n, float *A, float *C )
{
  //for(int i = 0; i < m; i++)
    //for(int k = 0; k < n; k++){
      //int km = k*m;
      //float* a_i = &A[i+km];
      //for(int j = 0; j < m; j++)
      	//C[i+j*m] += *a_i * A[j+km];
    //}
  for(int k = 0; k < n; k++){
    int km = k*m;
      for(int j = 0; j < m; j++){
      	int jm = j*m;
        float* a_j = &A[j+km];
        for(int i = 0; i < m; i++)
          C[i+jm] += A[i+km] * *a_j;
    }
  }
}
*/

/*
//Original Dgemm For Comparison
void dgemm( int m, int n, float *A, float *C )
{
  for(int i = 0; i < m; i++)
    for(int k = 0; k < n; k++) 
      for(int j = 0; j < m; j++) 
        C[i+j*m] += A[i+k*m] * A[j+k*m];
}
*/
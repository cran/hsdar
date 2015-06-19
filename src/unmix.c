#include <R.h>
#include <Rmath.h>
#include <R_ext/Lapack.h>
#include "c2lablas.h"

/*
This function solves the linear spectral unmixing equation via minimizing 
the least square error:

  partitions = em_matrix * al_vector

where
  em_matrix = endmember reflectance values
  al_vector = albedo (pixel) values
  partitions = vector containing fraction of endmember per endmember class

Note: The function is using the code origionally developped by Markus 
      Neteler for GRASS GIS version 5
*/

#define GAMMA 10

/*
##########################################################################################
#                                                                                        #
#                   Transpose matrix                                                     #
#                                                                                        #
##########################################################################################
Routine to transpose matrix given as vector of length = col * row.
  col = Number of columns
  row = Number or rows
  data = Matrix to be transposed
  data_t = Transposed matrix
##########################################################################################
*/

void transpose_matrix (int col, int row, double *data, double *data_t)
{
  int i, k;
  
  for (k=0; k<col; k++)
  {
    for (i=0; i<row; i++)
    {
//       printf("(%d,%d): %d -> %d\n",k+1,i+1,1+i + k * row, 1+k + i * row);
      data_t[k + i * col] = data[i + k * row];
    }
  }
}

void v_sub (int n, double *v1, double *v2, double *v3)
{
  int i;
  
  for (i=0; i<n; i++)
  {
    v3[i] = v1[i] - v2[i];
  }
}

void sv_mlt(int n, int k, double s, double *v1, double *v2)
{
  int i, m=n*k;
  
  for (i=0; i<m; i++)
  {
    v2[i] = v1[i] * s;
  }
}

void euclidian_norm (int n, double *ve, double *eucl)
{
  int i;
  double norm;
  
  norm =0.0;
  
  for (i=0; i<n; i++)
  {
    norm = norm + pow(ve[i],2);
  }
  norm = pow(norm, 0.5);
  
  *eucl=norm;    
}

void unmix_vec (int n_em, int n_band, int i_spec, double em_matrix_tilde[n_em * (n_band + 1)], 
                double em_matrix_tilde_t[n_em * (n_band + 1)], double al_vector[n_band], 
                double partitions[n_em], double *error, int quiet, int *min_reached_p)
{
  int i, k, n_bandp1;
  
  double mu;
  double startvector[n_em], startvector_prev[n_em];
  double A_times_startvector[n_band + 1];
  double errorvector[n_band + 1];
  double temp[n_em];
  double temp2[n_em];
  double A_tilde_trans_mu[n_em * (n_band + 1)];
  double b_gamma[n_band + 1];
  
  int iterations, min_reached, n_ch, mu_changed;
  double change, deviation, deviation2, *deviation2_p;
  
  int *n_bandp1_pointer, *n_em_pointer;
  
//   Initialisation

  mu = 0.000001*pow(10,*min_reached_p);  /* Step size */
    

  for (i = 0; i < n_band; i++)
  {
    b_gamma[i] = al_vector[i]; 
  }
  /* add GAMMA for 1. constraint as last element*/
  b_gamma[n_band] = GAMMA; 
  
  
  for (k = 0; k < n_em; k++)  /* no. of spectra times */
  {
    startvector[k] = (1. / n_em);
    startvector_prev[k] = (1. / n_em);
    temp2[k] = 1.0;
  }
  
  
  n_bandp1 = n_band + 1;
  
  n_bandp1_pointer = &n_bandp1;
  n_em_pointer = &n_em;
  deviation2_p = &deviation2;

  /* initialize */
  change=1000.;  
  
  deviation=1000;
  deviation2 = 0.0;
  iterations = 0;
  min_reached = 0;
  n_ch = 0;
  mu_changed = 1;
  
  while (min_reached == 0)
  {    
    F77_CALL(c2dgemv)(n_bandp1_pointer, n_em_pointer, em_matrix_tilde, startvector, A_times_startvector);
    
    v_sub(n_bandp1, A_times_startvector, b_gamma, errorvector);
    
    euclidian_norm(n_bandp1, errorvector, deviation2_p);
    
    if (deviation < deviation2)
    {
      mu = mu*0.9;
      n_ch = 0;
      mu_changed = 1;
      if (mu < 1.0e-10)
        min_reached = -1;
      for (k = 0; k < n_em; k++)
        startvector[k] = startvector_prev[k];
    } else {
      
      if (mu_changed == 1)
      {
        sv_mlt(n_bandp1, n_em, mu, em_matrix_tilde_t, A_tilde_trans_mu);
        mu_changed = 0;
      }
    
      F77_CALL(c2dgemv)(n_em_pointer, n_bandp1_pointer, A_tilde_trans_mu, errorvector, temp);
    
      v_sub(n_em, startvector, temp, temp2);
      
      i=0;
      for (k = 0; k < n_em; k++)
      {
        startvector_prev[k] = startvector[k];
        if (temp2[k] < 0)
        {
          startvector[k] = 0.0;
          i++;
        } else {
          startvector[k] = temp2[k];
        }
      }
      if (i==n_em)
        min_reached = -3;
      change = deviation - deviation2;
      deviation = deviation2;
      
      if (fabs(change) < 0.00000001)
      {
        n_ch++;
        if (n_ch > 10)
          min_reached = 1;
      } else {
        n_ch = 0;
      }
      iterations++;
      if (iterations == 100000)
        min_reached = -2;
    }
  }
  

//   Write result
  euclidian_norm(n_bandp1, b_gamma, deviation2_p);
  error[i_spec] = deviation / deviation2;
  for (k=0; k<n_em; k++)
    partitions[k] = startvector[k];
  *min_reached_p = min_reached;
}

void unmix (int *n_em, int *n_band, int *n_spec, double *em_matrix, double *spec_vector,
            double *fractions, double *error)
{
  int icol, irow, quiet;
  double em_matrix_tilde[*n_em * (*n_band + 1)];
  double em_matrix_tilde_t[*n_em * (*n_band + 1)];
  double al_vector[*n_band];
  double partitions[*n_em];
  double errorval;
  int sucess, su, *sucess_p;
  
  quiet = 1;
  sucess_p = &su;
  
//   Create overdetermined system by adding additional row containing GAMMA values to endmember matrix
  for (icol = 0; icol < *n_em; icol++)
  {
    for (irow = 0; irow < *n_band; irow++)
    {
      em_matrix_tilde[irow + icol * *n_band + icol] = em_matrix[irow + icol * *n_band];
    }
  }
  
  for (icol = 0; icol < *n_em; icol++)
  {
    em_matrix_tilde[*n_band * icol + *n_band + icol] = GAMMA;
  }
  
//   Transpose overdetermined matrix
  transpose_matrix(*n_em, *n_band + 1, em_matrix_tilde, em_matrix_tilde_t);
  
//   Loop over all spectra
  for (icol=0; icol<*n_spec; icol++)
  {
//     Initialize values    
    for (irow=0; irow<*n_band; irow++)
      al_vector[irow] = spec_vector[icol* *n_band + irow];
    for (irow=0; irow<*n_em; irow++)
      partitions[irow] = 0.0;
    errorval = 1.0;
    su=5;
    sucess=5;
    while (sucess > 0)
    {
      unmix_vec(*n_em, *n_band, icol, em_matrix_tilde, em_matrix_tilde_t, al_vector, 
                partitions, error, quiet, sucess_p);
      if (*sucess_p>-3)
      {
        sucess = 0;
      } else {
        sucess--;
        su = sucess;
      }
    }
    
    for (irow=0; irow<*n_em; irow++)
    {
      fractions[icol* *n_em + irow] = partitions[irow];
    }
  }
}

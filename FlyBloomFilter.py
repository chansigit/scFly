import numpy as np
from numpy.random import default_rng
from scipy.sparse import dok_matrix
import math,time
from math import ceil

def SB(d, m, rate,rand_seed=42):
    '''
    SB: generate a sparse and binary projection matrix
    return a matrix with shpae (d,m)
    in each column, only |d*rate| values are one
    @param d: dimensionality of data
    @param m: length of bloom filter
    @param rate: sampling rate, between 0 and 1
    '''
    # Build an empty sparse matrix of d-row, m-column
    M = dok_matrix((d, m), dtype=np.float32)
    
    # Count of elements set to 1 per column
    ones_per_col = math.ceil(rate*d)
    
    rng = default_rng(rand_seed)
    # Column-wise setting random ones
    for col_idx in range(m):
        ## randomly choose rows
        row_idx = rng.choice(d, size=ones_per_col, replace=False)
        ## and set them to one
        M[row_idx, col_idx]=1
    
    return(M)

class FlyBloomFilter():
    def __init__(self, reference, length, sampling_rate, topK):
        self.X = reference
        self.d = reference.shape[1]
        self.m = length
        self.sampling_rate= sampling_rate
        self.M = SB(d=self.d, m=self.m, rate=sampling_rate)
        self.B = np.ones(self.m)
        self.topK = topK
        self.loading_factor=0
        
        
    def remember(self):
        # apply random projection to compute Kenyon cell activities for input
        t1=time.time()
        self.KC = self.X @ self.M
        
        # reset bits corresponding to the top k indices
        hit_idx = (-self.KC).argsort()[:, :self.topK]
        #hit_idx =  np.asarray(hit_idx).flatten()
        #self.B[hit_idx]=0
        self.B[np.asarray(hit_idx)]=0
        
        self.loading_factor=sum(self.B==0)/self.m
        print(self.loading_factor)
        t2=time.time()
        print("time: %f s"%(t2-t1))
    
    def response(self, X):
        t1=time.time()
        response = X @ self.M
        
        hit_idx = (-response).argsort()[:, :self.topK]
        novelty=(self.B[np.asarray(hit_idx)].sum(axis=1)) / self.topK
        
        t2=time.time()
        print("time: %f s"%(t2-t1))
        return (novelty)

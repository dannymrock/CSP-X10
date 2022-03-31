# COP-X10: An X10 Implementation of the CPMH framework

This project containst the code for COP-X10, an implementation of the framework for Cooperative Parallel Metaheuristics.
Abstracting both the problem and the metaheuristic, our framework can tackle any COP with
any metaheuristic and provides hybridization mechanisms to combine several metaheuristics.
This framework includes several parameters to control the cooperation. In particular, they allow the user to tune the trade-off between intensification and diversification. CPMH can
be implemented on various parallel platforms (multicores, manycores, clusters, massively
parallel machines, GPU).

The implementation of the framework was developed using the X10 language designed by IBM. This parallel programming language allows us to experiment with different parallel hardware architectures and parallel topologies.
The current implementation supports only permutation problems and local search-based meta-heuristics. 


See [[1]](https://www.researchgate.net/profile/Danny-Munera/publication/261983770_A_Parametric_Framework_for_Cooperative_Parallel_Local_Search/links/0f3175360e9199545c000000/A-Parametric-Framework-for-Cooperative-Parallel-Local-Search.pdf)
and [[2]](https://www.researchgate.net/profile/Danny-Munera/publication/337891534_Solving_Hard_Combinatorial_Optimization_Problems_using_Cooperative_Parallel_Metaheuristics/links/5df0e1c892851c836473dc11/Solving-Hard-Combinatorial-Optimization-Problems-using-Cooperative-Parallel-Metaheuristics.pdf) for more details about the framework and the implementation. 

This project was developed as part of my Doctoral Thesis at the Centre de Recherche en Informatique (CRI), Université Paris 1 Panthéon-Sorbonne, France.

##Examples of use:
- mpirun -x LM=0 -x X10_NTHREADS=2 -n 32 Main -p CAP -s 18 -pf paramFiles/CAP.param -N 2 -R 1000 -U 2000 
- mpirun -x LM=0 -x X10_NTHREADS=2 -H node1,node2 -n 32 Main -p CAP -s 18 -pf paramFiles/CAP.param -N 2 -R 1000 -U 2000 

#!/bin/sh
# $Id: run.sh,v 1.1.1.1 2014/12/02 00:00:00 seiji Exp seiji $

exe=paraio
ncores=32

nmpi=${1:-$ncores}
nomp=${2:-1}
nppn=${3:-`expr $ncores / $nomp`}
# input parameters for IO
nword=${4:-100000000}
niter=${5:-1}

wkdir=wk.${nmpi}mpi-${nomp}omp-${nppn}ppn.`date +%Y%m%d_%H%M%S`
mkdir -p $wkdir
cd       $wkdir
ln -s ../$exe

cat << EOF > PBS
#!/bin/sh -x
#PBS -N PARAIO
#PBS -j oe
#PBS -l nodes=`expr $nmpi / $nppn`:ppn=$ncores
#PBS -l walltime=01:00:00

cd \$PBS_O_WORKDIR

#ulimit -s unlimited
#ulimit -c unlimited

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${CRAY_LD_LIBRARY_PATH}
export NO_STOP_MESSAGE=1
export OMP_NUM_THREADS=$nomp

/usr/bin/time -p aprun -n$nmpi -N$nppn -d$nomp -S`expr $nppn / 2` -j1 -ss -cc cpu ./$exe << END > run.log 2>&1
 $nword $niter
END

ls -l
rm -f IODATA.*

cat run.log

EOF

chmod a+x PBS
qsub PBS


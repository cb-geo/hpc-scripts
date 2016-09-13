# Abaqus HPC Darwin submission script
> Cambridge-Berkeley Geomechanics

Submission script to run abaqus on multiple nodes.

## Abaqus validation files (CC-by-Share-Alike-4.0)
* Wellbore  - Tsubasa Sasaki

# Running abaqus on Darwin
* Edit the submission file `submit` and add the project information:

        #! Name of the job:
        #SBATCH -J abaqus
        #! Which project should be charged:
        #SBATCH -A SOGA-SL2-AB123
        #! How many whole nodes should be allocated?
        #SBATCH --nodes=2
        #! How many (MPI) tasks will there be in total? (<= nodes*16)
        #SBATCH --ntasks=32
        #! How much wallclock time will be required?
        #SBATCH --time=02:00:00
        #! What types of email messages do you wish to receive?
        #SBATCH --mail-type=ALL
* Edit abaqus arguments in the submission file:
        
        CMD="abaqus interactive job=jobname user=umatfile cpus=32"
  * `job=jobname`, where jobname is the name of the `*.inp` file.
  * `user=umatfile`, this is the V/UMAT file. Should have a `.f` extension. On Windows OS this file will have a `.for` extension.
  * `cpus=XX`, where XX is the total `ntasks`, i.e., number of nodes * 16.

* Edit the abaqus_v6.env line in the submission file:
        
        printf '%s\n%s' "cpus=32" "mp_host_list=`generate_abaqus_nodefile`" > $workdir/abaqus_v6.env
  * `cpus=XX`, where XX is the total `ntasks`, i.e., number of nodes * 16.

* To submit the abaqus job, run: `sbatch submit`. Ensure all input files and user defined codes are in teh same directory.

# Speedup

TL;DR: **Use 4 Darwin nodes** for optimal performance.

Abaqus speed-up comparison for the wellbore validation file and total cost for one analysis. Looking at the cost increase and the amount of speed-up increase we get. The 4 nodes 64 threads seems the best option. Going for 8 nodes increases the cost by 54% but we don't get that much of a speed-up. I recommend using 4 nodes with 64 threads.

Model 	      | # cores | Time(minutes)| Speedup | Cost increase | Efficiency
---------- | ----- |-----------| ----------|---------| -------------
Original | 8 | 180 | 1x | -- | --
Darwin (1 node) | 16 | 109 | 1.65x (1x) | 1x | 1.00
Darwin (2 nodes) | 32 | 75 | 2.4x (1.45x) | 1.35x | 1.07
**Darwin (4 nodes)** | **64** | **49** | **3.8x (2.3x)** | **1.78x** | **1.29**
Darwin (8 nodes) | 128 | 38 | 4.75x (2.9x) | 1.62x | 1.05

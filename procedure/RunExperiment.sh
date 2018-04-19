subject_nr=$(cat ~/github/sft-fraction/procedure/subject_nr.txt)
next_subject=$(($subject_nr + 1))

echo "Subject number = $subject_nr"
echo $next_subject > ~/github/sft-fraction/procedure/subject_nr.txt

echo "Starting experiment..."
opensesamerun ~/github/sft-fraction/procedure/experiment.osexp -s $subject_nr -l "/home/mathcog/github/sft-fraction/results/data/subject_$subject_nr.csv"

echo Experiment complete!
echo Goodbye.

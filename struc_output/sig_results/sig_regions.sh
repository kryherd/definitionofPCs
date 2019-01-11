for model in `tail -n +2 ../../model_names.csv`
do
	model2=`echo ${model} | sed -e 's/^"//' -e 's/"$//'`
	line=`python extract_region_names.py --model ${model2}`
	echo "$line" >> ${model2}-sig_areas.txt
done
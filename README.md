# motion-feedback

## Task design and procedure

Our aim in this study was to see whether giving participants in fMRI studies real-time feedback about their head motion would reduce head motion.

Participants performed a word repetition task in which on every trial they heard a spoken word, presented in background noise, and repeated it back.

There were 6 runs, and thus for each subject, 6 different time series of data.

We used SPM to perform motion correction on the functional data, resulting in 6 motion parameters (translation around the x, y, z axes; rotation around the x, y, z aces). Scan-to-scan, or frame-to-frame, movement is summarized using framewise displacement (FD), a metric that incorporates all 6 canonical motion parameters to a single number. For each scan, we provide FD values.




## Files

 * In `data` is `motion_data.csv`, which contains one row per time point per subject. It also has `participants.csv` that has information on...well, participants (age, sex, whether they received feedback or not)
 * `analyses` has R scripts for analysis and plotting

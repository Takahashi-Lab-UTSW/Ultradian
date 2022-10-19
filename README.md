# Ultradian
Raw data & Scripts.

Due to space limitation, some raw data and all the image stacks are not included, but are available upon request.

Analysis of DeltaVision image stack series -:
Single cell:
1. Copy “UC_Track.jar” to ImageJ/plugins folder
2. Open ImageJ, from Plugins manu select “UC Track”
3. Three windows will appear: “Time Series V3_0”, “ROI Manager”, and “Channels for Substacking”. On the “Channels for Substacking” window, input the number of channels to split then click “OK”, then input the names of the channels and click “OK”.
4. The “Open dv file” window will popup. Select the dv file to be analyzed then click “open”. The program will split the channels, subtract background by rolling ball algorithm, and save the splited files to the same folder.
5. Select cells and add the ROIs to ROI Manager.
6. Check the “Track” checkbox if the cells move substantially, then from the “Track by channel” dropbox select a channel to track the cells.
7. Click “Get Average” on the “Time Series V3_0” window. The average intensities for each cell in each channel across the image series will be saved to the same folder as a csv file.

Whole image: 
1. Put “IJ-macro_DV-split-norm.java” file to the same folder as the dv files (optional).
2. Open ImageJ, from Plugins manu select “Macros -> Run”.
3. The “Run Macro or Script” window appears. Select “IJ-macro_DV-split-norm.java” file and click “Open”.
4. The “Choose a Directory” window popup, choose the folder and click “Select”.
5. The “Channel#” window appears, input the number of channels in the dv files.
6. In the next window give each channel a convenient name, click OK.
7. For each dv file a new folder is created with the file name followed by “_Split”. In this folder the splited image series (raw and background subtracted) were saved. The average intensity of the background-subtracted whole image was saved to a csv file.


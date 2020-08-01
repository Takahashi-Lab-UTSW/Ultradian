
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import ij.*;
import ij.process.*;
import ij.gui.*;
import ij.io.Opener;
import ij.measure.ResultsTable;
import ij.plugin.PlugIn;
import ij.plugin.filter.BackgroundSubtracter;
import ij.plugin.frame.*;
import java.awt.datatransfer.*;
import java.io.File;
import java.io.FileWriter;
import javax.swing.*;


/* 
Developed from Time Series Analysis Version 3_3 (06th Aug 2008) by @author balaji

*/
public class UC_Track extends PlugInFrame implements ActionListener, MouseListener, ItemListener,
ClipboardOwner/**/, PlugIn, KeyListener/* for keyborad shortcut*/,Runnable,ImageListener{
    Panel panel;
    static Frame Instance;

    protected double MeanIntensity[] = null;
    protected double Err[] = null;
    private boolean ADD = false;
    private String Names[] = {"Rectangle","Oval","FreeHand (not implemented)"};
    private int ROIType = 1;
    private int Width = 20;
    private int Height = 20;
    private Roi AutoROI = new OvalRoi(0,0,Width,Height);
    private ShapeRoi all = new ShapeRoi(AutoROI);
    private int MaxIteration = 15;
    private double CLimit = 0.1;
    private double MagCal = 0.5;
    private boolean ReCtrMean = false;
    private boolean Label = true;
    private  ResultsTable rt;
    private PlotWindow graph;
    private int ROICount = 0;
    private String Prefix = new String("ROI");
    private Thread  thread;
    boolean done = false;
    private ImageCanvas previousCanvas = null;
    private boolean KeepPrefix = true;
    private int uiMeasure = 2; //2 for pixel average and 3 for integrated intensity;
    ImagePlus previousImp = null, processedImp = null;
    ImageStack AveStack = null ;
    java.awt.Checkbox AddOnClick, UpdateStack, persist, LiveGraph,Track;
    JComboBox TrackCB;
    Opener opener = new Opener();  
    String name, DIR;
    
    public UC_Substack subs =new UC_Substack();
    public Vector chs = subs.getChs();

    public void lostOwnership (Clipboard clip, Transferable cont) {} 

    public void setIntegratedIntensity(boolean IntegratedIntensity){
        uiMeasure = (IntegratedIntensity) ? 3 : 2;
    }

    public void run() {
        subs.open();

        DIR=subs.dir;
        name=subs.name;

        while (!done) {
            try {Thread.sleep(500);}
            catch(InterruptedException e) {}
            ImagePlus imp = WindowManager.getImage(name+chs.get(0));
            if (imp != null){
                    ImageCanvas canvas = imp.getCanvas();
                    if (canvas != previousCanvas){
                            if(previousCanvas != null)
                                   previousCanvas.removeMouseListener(this);
                            canvas.addMouseListener(this);
                            previousCanvas = canvas;
                    }
            }
            else{
                if(previousCanvas != null)
                    previousCanvas.removeMouseListener(this);
                previousCanvas = null;
            }    
        }
    }
        
    public void subtractBackground(){
        BackgroundSubtracter bgs = new BackgroundSubtracter();
        String[] wList = WindowManager.getImageTitles();

        for (int w=0;w<wList.length;w++){
            ImagePlus imp = WindowManager.getImage(wList[w]);
            int nSlice = imp.getNSlices();
            ImageStack stack = new ImageStack(imp.getWidth(),imp.getHeight());
            for (int slice=1; slice<=nSlice; slice++){
                IJ.showStatus("Normalizing: "+slice+"/"+nSlice);
                IJ.showProgress((double)slice/nSlice);
                imp.setSlice(slice);
                ImageProcessor ip = imp.getProcessor();
                bgs.rollingBallBackground(ip, 50, false, false, false, true, true); //subtract background rolling ball 50pix
                stack.addSlice(null,ip);
            }
            ImagePlus imp1=new ImagePlus(wList[w], stack);
            imp.close();
            ImageProcessor ip1 = stack.getProcessor(1);
            autoAdjust(imp1, ip1);
            imp1.show();
        }
    }
    
    void autoAdjust(ImagePlus imp, ImageProcessor ip) {
        ij.process.ImageStatistics stats = new ImageStatistics();
        stats = imp.getStatistics();
        int limit = stats.pixelCount/10;
        int[] histogram = stats.histogram;
    
        int autoThreshold = 5000;
        double min, max; 
        int threshold = stats.pixelCount/autoThreshold;
        int i = -1;
        boolean found = false;
        int count;
        do {
            i++;
            count = histogram[i];
            if (count>limit) count = 0;
            found = count> threshold;
        } while (!found && i<255);
        int hmin = i;
        i = 256;
        do {
            i--;
            count = histogram[i];
            if (count>limit) count = 0;
            found = count > threshold;
        } while (!found && i>0);
        int hmax = i;
        Roi roi = imp.getRoi();
        if (hmax>=hmin) {
            //if (RGBImage) imp.deleteRoi();
            min = stats.histMin+hmin*stats.binSize;
            max = stats.histMin+hmax*stats.binSize;
            if (min==max)
                {min=stats.min; max=stats.max;}
            setMinAndMax(imp, min, max);
            //if (RGBImage && roi!=null) imp.setRoi(roi);
        } else {
            reset(imp, ip);
            return;
        }
    }

    void setMinAndMax(ImagePlus imp, double min, double max) {
        boolean rgb = imp.getType()==ImagePlus.COLOR_RGB;
            imp.setDisplayRange(min, max);
    }

    void reset(ImagePlus imp, ImageProcessor ip) {
        int bitDepth = imp.getBitDepth();
        double defaultMin=0, defaultMax=255;
        if (bitDepth==16 || bitDepth==32) {
            imp.resetDisplayRange();
            defaultMin = imp.getDisplayRangeMin();
            defaultMax = imp.getDisplayRangeMax();
            //plot.defaultMin = defaultMin;
            //plot.defaultMax = defaultMax;
        }
        double min, max;
        min = defaultMin;
        max = defaultMax;
        setMinAndMax(imp, min, max);
        //updateScrollBars(null, false);
        //plotHistogram(imp);
        //autoThreshold = 0;
    }
    
    RoiManager getManager() {
        RoiManager instance = RoiManager.getInstance();
        if (instance == null)
            return new RoiManager ();
        else
            return instance;
    }

    public void windowClosed(WindowEvent e) {
        Instance = null;
        done = true;
        AddOnClick.setState(false);
        ROICount = 0;
        all = null;
        AutoROI = null;
        ImagePlus.removeImageListener(this);
        ImagePlus imp = WindowManager.getCurrentImage();
        //ImageWindow Win = imp.getWindow();
        if (imp == null){
            previousCanvas = null;
            super.windowClosed(e);
           return;
        }
        ImageCanvas canvas = imp.getCanvas();
        if(canvas != null){
            canvas.removeMouseListener(this);
            canvas.removeKeyListener(this);
        }
        if(previousCanvas != null)
            previousCanvas.removeMouseListener(this);
        previousCanvas = null;
        super.windowClosed(e);
    }
   
    public UC_Track() {
        super ("Time Series V3_0");
        if (Instance != null){
            Instance.toFront();
        }
        else{
            Instance = this;
            ImagePlus.addImageListener(this);
            WindowManager.addWindow(this);
            setLayout(new FlowLayout(FlowLayout.CENTER,5,5));

            panel = new Panel();
            panel.setLayout(new GridLayout(12, 0, 0, 0));
           
            addButton("Auto ROI Properties");
//            addButton("Recenter");
            addButton("Recenter Parameters");
            
            addButton("Get Average"); //Average over all the ROIs
            addButton("Get Total Intensity");
            addButton("Reset");
           
            addButton("Subtract Background");
            //addButton("SetasAutoROi");
            
            panel.add(Track = new Checkbox("Track", false));
            JLabel trk = new JLabel ("Track by channel:");
            panel.add(trk);
            panel.add(TrackCB = new JComboBox(chs));
            
            AddOnClick = new Checkbox("Add On Click");
            panel.add(AddOnClick);
            AddOnClick.setState(false);
            panel.add(persist = new Checkbox("Persist", true));
//            panel.add(LiveGraph = new Checkbox("Live Graph", false));
            panel.add(UpdateStack = new Checkbox("New thread for measuring", true));
            add(panel);
            pack();
            //GUI.center(this);
            this.setVisible(true);
            thread = new Thread(this,"Time Series ");
            thread.setPriority(Math.max(thread.getPriority()-2,thread.MIN_PRIORITY));
            thread.start();
            getManager();
        }
    }
    void addButton(String label) {
            Button b = new Button(label);
            b.addActionListener(this);
            panel.add(b);
    }
    public void actionPerformed(ActionEvent e) {
            String label = e.getActionCommand();
            if (label==null)
                    return;
            String command = label;
            if(command.equals("Auto ROI Properties"))
                SetAutoROIProperties();
//                if(command.equals("Recenter"))
//                    recenter();
            if(command.equals("Recenter Parameters"))
                SetRecenterProp();
            if(command.equals("Get Average"))
//                    showDialog_Combine();

                getAverage();

//                if(command.equals("Get Total Intensity"))
//                    this.getIntegrated();
            if (command.equals("Reset")) {
                if (KeepPrefix)
                    ResetNum();
                else
                    RenameROIS();
            }
            if(command.equals("Subtract Background")){
                subtractBackground();
//                    MoveRois();
            }
            if(command.equals("SetasAutoROi"))
                DefAutoROi();
    }
    
    protected void DefAutoROi(){
        IJ.showMessage("Yet to be Implemented");

    }
        
    protected void MoveRois(){
        GenericDialog gd = new GenericDialog("Translate ROi's");
        gd.addNumericField("Enter the y shift(negative would move the ROis up)",0,0);
        gd.addNumericField("Enter the x shift (negative would move the ROis left)",0,0);
        gd.showDialog();
        int xShift = 0, yShift = 0;
        if (gd.wasCanceled())
            return;
        RoiManager manager = getManager();
        Roi[] rois = manager.getSelectedRoisAsArray();
        if (rois.length == 0) {
            IJ.showMessage("No rois in the ROI manager");
            return;
        }
        yShift = (int)gd.getNextNumber();
        xShift = (int)gd.getNextNumber();
        java.awt.Rectangle BRect ;
        Roi CurRoi,tmpRoi;
        int NewX, NewY;
        for(int i = 0 ; i < rois.length ; i++){
           CurRoi = rois[i];
           BRect = CurRoi.getBounds();
           NewX = Math.round(BRect.x  + xShift);
           NewY = Math.round(BRect.y  + yShift);
           CurRoi.setLocation(NewX,NewY);
         }
        manager.runCommand("show all");
     }

    public void itemStateChanged(ItemEvent e) {
            // Want to use it for dynamically updating the profile. Will be addresssed in later version
    }
      
    public void keyPressed(KeyEvent e) {}
    public void keyReleased (KeyEvent e) {}
    public void keyTyped (KeyEvent e) {}
    public void mousePressed(MouseEvent e){}
    public void mouseReleased(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}

    public void mouseClicked(MouseEvent e) {

        if (AddOnClick.getState()){
            int x = e.getX();
            int y = e.getY();

            ImagePlus imp = WindowManager.getCurrentImage();
            if (imp != null){
                ImageWindow Win = imp.getWindow();
                ImageCanvas canvas = Win.getCanvas();

                int offscreenX = canvas.offScreenX(x);
                int offscreenY = canvas.offScreenY(y);
                int Start_x = offscreenX - (int)(Width/2);
                int Start_y = offscreenY - (int)(Height/2);
                AutoROI.setLocation(Start_x, Start_y);                
                ROICount++;
                imp.setRoi(AutoROI);
                String name;
                if (ROICount < 100){
                     name = (ROICount < 10 ) ? Prefix + "00" + ROICount :  Prefix + "0"  + ROICount;       
                }
                else{
                    name = Prefix + ROICount;
                }
                //ROIList.add(name);
                Roi temp = (Roi)AutoROI.clone();
                temp.setName(name);
                 getManager().addRoi(temp);
                if (persist.getState())
                    getManager().runCommand("show all");
               // if(LiveGraph.getState()){
                  //  getAverage();
                //}
               /* if(DisplayLabel.getState()){
                    AddLabel(temp, name);
                }*/
            } 
        }
    }

    public void ResetNum() {
        int[] indexes = getSelectedIndexes();
        if (indexes.length == 0) {
            IJ.showMessage("No rois in the ROI manager");
            return;
        }
        String Label = "";
        RoiManager manager = getManager();
        Roi[] rois = manager.getRoisAsArray();
        for (int i = 0 ; i < indexes.length ; i++,++ROICount){
            Roi tmpRoi =  rois[indexes[i]];
            Label = tmpRoi.getName();
            Label = Label.substring(0,(Label.length()-2));
            if (ROICount < 100)
                 Label = (ROICount < 9 ) ? Prefix + "00" + (ROICount+1) :  Prefix + "0"  + (ROICount+1);
            else
                Label = Prefix + (ROICount +1);
            manager.select(indexes[i]);
            manager.runCommand("Rename",Label);
        }
        manager.runCommand("deselect");
        manager.runCommand("show all");
    }
        
    public void RenameROIS(){
        ROICount = 0;
        int[] indexes = getSelectedIndexes();
        if (indexes.length == 0) {
            IJ.showMessage("No rois in the ROI manager");
            return;
        }
        String Label = "";
        RoiManager manager = getManager();
        Roi[] rois = manager.getRoisAsArray();
        for (int i = 0 ; i < indexes.length ; i++,++ROICount){
            Roi tmpRoi =  rois[indexes[i]];
            if (ROICount < 100)
                 Label = (ROICount < 9 ) ? Prefix + "00" + (ROICount+1) :  Prefix + "0"  + (ROICount +1);
            else
                Label = Prefix + (ROICount+1);
            manager.select(indexes[i]);
            manager.runCommand("Rename",Label);
        }   
        manager.runCommand("deselect");
        manager.runCommand("show all");
     }

    public void mouseEntered(MouseEvent e) {}

    // This method is reqd. for the button interface
    public void SetAutoROIProperties(){
        ij.gui.GenericDialog gd = new ij.gui.GenericDialog("AutoROI properties");
        gd.addNumericField("Width: ", Width, 0);
        gd.addNumericField("Height: ", Height, 0);
        gd.addNumericField("Start the ROI number from",ROICount, 0);
        gd.addStringField("Prefix for AutoROI",Prefix);
        // boolean values[] = {false,true,false};
        gd.addChoice("ROI Type",Names,Names[ROIType]);
        gd.addCheckbox("Resize exisiting ROIS", false);
        gd.addCheckbox("Keep the prefix during reset",KeepPrefix);
        gd.showDialog();

        if(!gd.wasCanceled())
        {
            this.Width = (int)gd.getNextNumber();
            this.Height = (int)gd.getNextNumber();
            int Count = (int)gd.getNextNumber();
            if(Count != ROICount && Count > 1) ROICount = Count - 1;              // >1 is an indication the number is last ROI in the manager
            //IJ.log("New ROI"+ Width + " "+ Height); //for debugging
            this.Prefix = gd.getNextString();
            ROIType = gd.getNextChoiceIndex();
            switch (ROIType) {
            case 0:
                    this.AutoROI = new Roi(0,0,Width,Height);
                    break;
            case 1:
                    this.AutoROI = new OvalRoi(0,0,Width,Height);
                    break;
            }
            if(gd.getNextBoolean()){
                ResizeROIS();
            }
            KeepPrefix = gd.getNextBoolean();
        }
    }
    public void ScaleROIS(double Scale){
            Width = (int)(Width * Scale);
            Height = (int)(Height * Scale);
            switch (ROIType) {
            case 0:
                    this.AutoROI = new Roi(0,0,Width,Height);
                    break;
            case 1:
                    this.AutoROI = new OvalRoi(0,0,Width,Height);
                    break;
            }
            ResizeROIS();
    }

    public void ScaleROIS(int Width, int Height){
            this.Width = Width ;
            this.Height = Height ;
            switch (ROIType) {
            case 0:
                    this.AutoROI = new Roi(0,0,Width,Height);
                    break;
            case 1:
                    this.AutoROI = new OvalRoi(0,0,Width,Height);
                    break;
            }
            ResizeROIS();
    }

    public void ResizeROIS(){
         RoiManager manager = getManager();
         Roi[] rois = manager.getSelectedRoisAsArray();
         if (rois.length == 0) {
             IJ.showMessage("No rois in the ROI manager");
             return;
         }
        java.awt.Rectangle BRect ;
        Roi CurRoi,tmpRoi;
        int NewX, NewY;
        for(int i = 0 ; i < rois.length ; i++){
            CurRoi = rois[i];
            BRect = CurRoi.getBounds();
            NewX = Math.round(BRect.x + (BRect.width - Width)/2);
            NewY = Math.round(BRect.y + (BRect.height - Height)/2);
            CurRoi.setLocation(NewX,NewY);
            CurRoi.setName(CurRoi.getName());
        }
        manager.runCommand("show all");
    }

    public void SetRecenterProp(){
        ij.gui.GenericDialog gd = new ij.gui.GenericDialog("Recentering Properties");

        gd.addNumericField("Convergence Limit (Pixels) ", CLimit, 1);
        gd.addNumericField("Maximum Iterations: ", MaxIteration, 0);
        gd.addNumericField("Rescale ROI by ",MagCal,1);

        gd.addCheckbox("Recenter for measuring mean",ReCtrMean);
        gd.showDialog();
        if(!gd.wasCanceled())
        {
            CLimit = gd.getNextNumber();
            MaxIteration = (int)gd.getNextNumber();
            MagCal = gd.getNextNumber();
            ReCtrMean = (boolean) gd.getNextBoolean();
        }
    }

    private Vector rois_trkd = new Vector();
    public void recenter(ImagePlus imp, Roi roi, int CurSlice){

        if(imp != null){
            ImageStatistics stat = new ImageStatistics();
            ij.measure.Calibration calib = imp.getCalibration();
            double xScale = calib.pixelWidth;
            double yScale = calib.pixelHeight;
//                ShapeRoi temp = null;
            boolean Converge = false;
            int New_x = 0;
            int New_y = 0;
            imp.setSlice(CurSlice);
            double xMovement = 0, yMovement = 0;
            java.awt.Rectangle Boundary;
//                for(int i = 0; i < rois.length ; i++){
                    Roi CurRoi =  roi;
                    Boundary = CurRoi.getBounds();
                    Converge = false;
                    imp.setRoi(CurRoi);
                    double OldDiff = 0,NewDiff = 0;
                    int Old_x,Old_y;
                    for(int Iteration = 1 ; Iteration <= MaxIteration  && !Converge; Iteration++){
                        stat = imp.getStatistics(64 + 32); //Calculate center of Mass and Centroid; 
                        New_x = (int) Math.round(((stat.xCenterOfMass/xScale) - (Boundary.getWidth()/2.0)));
                        New_y = (int) Math.round(((stat.yCenterOfMass/yScale) - (Boundary.getHeight()/2.0)));
                        // Calculate movements
                        xMovement =(stat.xCentroid - stat.xCenterOfMass)/xScale;
                        yMovement = (stat.yCentroid - stat.yCenterOfMass)/yScale;
                        if( Math.abs(xMovement) < 1 && xMovement != 0 && yMovement != 0 && Math.abs(yMovement) < 1){ //Now search nearby;
                            if(Math.abs(xMovement) > Math.abs(yMovement)){
                                New_x = (xMovement > 0) ? (int)Math.round(stat.xCentroid/xScale - (Boundary.getWidth()/2.0) - 1) : (int)Math.round(stat.xCentroid/xScale - (Boundary.getWidth()/2.0) + 1);
                                New_y = (int) Math.round(stat.yCentroid/yScale - (Boundary.getHeight()/2.0));
                            }
                            else{
                                New_y = (yMovement > 0) ? (int)Math.round(stat.yCentroid/yScale -(Boundary.getHeight()/2.0)- 1) : (int)Math.round(stat.yCentroid/yScale - (Boundary.getHeight()/2.0)+ 1);
                                New_x = (int) Math.round(stat.xCentroid/xScale -(Boundary.getWidth()/2.0));
                            }
                        }
                        else{
                            New_x = (int)Math.round (((stat.xCenterOfMass/xScale) - (Boundary.getWidth()/2.0)));
                            New_y = (int)Math.round (((stat.yCenterOfMass/yScale) - (Boundary.getHeight()/2.0)));

                        }
                        Converge = ( Math.abs(xMovement) < CLimit && Math.abs(yMovement) < CLimit)  ? true : false ;
                        CurRoi.setLocation(New_x ,New_y);
                        imp.setRoi(CurRoi);
                    }
//                         temp = new ShapeRoi(CurRoi);
//                        all = (i == 0) ? new ShapeRoi(CurRoi) : all.xor(temp);

                   /* if(!Converge) 
                            IJ.log(indexes[i] + "\t ROI did not converge" );*/
                   /* else
                           IJ.log(indexes[i] + "\t ROI converged" );*/
                    rois_trkd.add(CurRoi);
//                }
//               ScaleROIS(CurROIWidth,CurROIHeight);

        } 

    }        
      
    public int[] getSelectedIndexes() {
       int[] indexes = getManager().getSelectedIndexes();
       if (indexes==null || indexes.length==0)
           indexes = getAllIndexes();
            return indexes;
    }

    public int[] getAllIndexes(){
            int count = getManager().getCount();
            int[] indexes = new int[count];
            for (int i=0; i<count; i++)
                    indexes[i] = i;
            return indexes;
    }

    public void getAveWithoutUpdate(boolean DispRes){
        ImagePlus imp = WindowManager.getCurrentImage();
        if (imp != null){
            RoiManager manager = getManager();
            Roi[] rois = manager.getSelectedRoisAsArray();
            if (rois.length == 0) {
                IJ.showMessage("You need to add atleast one ROI");
                return;
            }
            if(rois.length > 148){
                IJ.showMessage("Warning","Results table can  display 150 (148 ROis) columns only. Excess "+(rois.length - 148)+" ROis will be omitted");
            }
            ImageStatistics stat = new ImageStatistics();
            int MaxSlice = imp.getStackSize(); 
            if(MaxSlice < 2){
                IJ.showMessage("This plugin requires a ImageStack: ImageJ found" + MaxSlice + "slice only");
                return;
            }
            MeanIntensity = new double[MaxSlice];
            Err = new double[MaxSlice];
            //int StartSlice = imp.getCurrentSlice();
            String Mean = "";
            double Sum, SqSum, Variance;
            Roi roi;
            rt = new ResultsTable();
            ImageProcessor ip = imp.getProcessor();
            int nCol_Res_Tab = (rois.length > 147) ? 147 : rois.length;                           
            double Int = 0;
            imp.unlock();
            for (int CurSlice = 0 ; CurSlice < MaxSlice ; CurSlice ++){
                imp.setSlice(CurSlice+1);                
                Sum = 0;
                SqSum = 0;
                rt.incrementCounter();
//                    if(ReCtrMean){
//                        recenter(imp,CurSlice+1);
//                        imp.setSlice(CurSlice+1);
//                        imp.setRoi(all);
//                    }
               for (int CurIdx = 0; CurIdx < rois.length && CurIdx <= 147; CurIdx++){
                    roi = rois[CurIdx];
                    imp.setRoi(roi);
                    stat = imp.getStatistics(uiMeasure); // MEAN = 2
                    Int = (uiMeasure == 2) ? stat.mean : stat.mean *stat.area;
                    rt.addValue(roi.getName(),Int);
                    Sum += Int;
                    SqSum += (Int * Int) ;
                }
                MeanIntensity[CurSlice] = Sum/rois.length;
                Variance = ((SqSum/rois.length)- MeanIntensity[CurSlice]*MeanIntensity[CurSlice]);
                Err[CurSlice] = (true /*StdErr*/) ? java.lang.Math.sqrt(Variance/rois.length)
                                                                        : java.lang.Math.sqrt(Variance);
                rt.addValue("Average",MeanIntensity[CurSlice]);
                rt.addValue("Err",Err[CurSlice]);
            }

            if(DispRes){
                rt.show("Time Trace(s)");
                double [] xAxis = new double[MaxSlice];
                for(int nFrames = 1 ; nFrames <= MaxSlice ; nFrames++)
                   xAxis[nFrames-1] = nFrames; 
                Plot plot = new Plot("Time Trace Average","Time (Frames)","Average Intensity",xAxis,MeanIntensity);
                //plot.addErrorBars(Err);
                plot.draw();
                if(WindowManager.getImage("Time Trace Average")== null)
                    graph = null;
               if(graph == null){
                    graph = plot.show();
                    //graph.addErrorBars(Err);
                    graph.addPoints(xAxis,MeanIntensity,PlotWindow.CIRCLE);
                }
                else{
                    graph.drawPlot(plot);
                   // graph.addErrorBars(Err);
                    graph.addPoints(xAxis,MeanIntensity,PlotWindow.CIRCLE);
                }
            }
        }
        return;
    }

    public void getAverage() {
        if (UpdateStack.getState()){
            ImagePlus imp = WindowManager.getCurrentImage();
            if(imp != null){
                ImageStack Stack = imp.getStack();
                if(Stack.getSize() < 2){
                   IJ.showMessage("This function requires stacks with more than 1 slice"); 
                   return;
                }
            }   
            else {
                IJ.showMessage("OOPS! No images are open");
                return;
            }
            TimeTraceUC_Track_BTG Trace = new TimeTraceUC_Track_BTG(imp,getManager());
            Trace.setName("Trace");
            Trace.setPriority(Math.max(Trace.getPriority()-2,Trace.MIN_PRIORITY));
            Trace.start();
        } else {
            setIntegratedIntensity(false);
            getAveWithoutUpdate(true);
        }
    }

    public double[] getAverageData(){
        return (double[])MeanIntensity.clone();
    }

    public void showGraph(){
        IJ.showMessage("Not yet implemented");
    }
        
   
    private void getIntegrated() {
    //IJ.showMessage("Not yet Implemented");
        if(UpdateStack.getState()){
            ImagePlus imp = WindowManager.getCurrentImage();
            if(imp != null){
                ImageStack Stack = imp.getStack();
                if(Stack.getSize() < 2){
                   IJ.showMessage("This function requires stacks with more than 1 slice"); 
                   return;
                }
            }   
            else{
                IJ.showMessage("OOPS! No images are open");
                return;
            }
            TimeTraceUC_Track_BTG Trace = new TimeTraceUC_Track_BTG(imp,getManager());
            Trace.setName("Trace");
            Trace.setPriority(Math.max(Trace.getPriority()-2,Trace.MIN_PRIORITY));
            Trace.setTotIntensity(true);
            Trace.start();
        }
        else{
            this.setIntegratedIntensity(true);
            getAveWithoutUpdate(true);
        }
    }

    public void imageOpened(ImagePlus imp) {
    }

    public void imageClosed(ImagePlus imp) {
        imp.getCanvas().removeMouseListener(this);
        imp.getCanvas().removeKeyListener(this);
        //
        
    }

    public void imageUpdated(ImagePlus imp) {
        if (/*Label*/ true){
           // LabelROIs();
        }
    }
    

class TimeTraceUC_Track_BTG extends Thread{
    ImagePlus imp;
    RoiManager Manager;
    TraceDataUC_Track_BTG[] Data; 
    TraceDataUC_Track_BTG Average;
    TraceDataUC_Track_BTG Err;
    ResultsTable rt;
    double Variance;
    double MeanIntensity;
    double Error;
    boolean showAverage = true;
    boolean showAll = false;
    boolean CalAverage = true;
    PlotWindow graph;
    int uiMeasure = 2; // 2 for Pixel average and 3 for Total Intensity;

    public void setTotIntensity(boolean TotIntensity ){
       uiMeasure = (TotIntensity) ? 3 : 2;
    }

    public void setAverage(boolean x){
        CalAverage = x;
        if (!CalAverage)
            showAverage = false;
   }

    public void setDispAll(boolean x){
        showAll = x;
        if(x) 
            CalAverage = x;
    }

    public void setAveDisp(boolean x){
        showAverage = x;
        if(x) 
            CalAverage = x;
    }

    TimeTraceUC_Track_BTG(ImagePlus imp, RoiManager Manager){
        if (imp != null && Manager != null){
            this.imp = imp;
            this.Manager = Manager;
        }
    }
  
    public void run(){
        int ch = chs.size();
        Roi[] rois = getManager().getSelectedRoisAsArray();
        int nRoi=rois.length;
        if (rois.length == 0) {
            IJ.showMessage("You need to add atleast one ROI");
            return;
        }
        if (rois.length > 148){
            IJ.showMessage("Warning","Results table can  display 150 (148 ROis) columns only. Excess "+(rois.length - 148)+" ROis will be omitted");
        }
        ImagePlus imp1=(ImagePlus) WindowManager.getImage(name+chs.get(0));
        int MaxSlice = imp1.getStackSize();  
        
        ImagePlus imptk= new ImagePlus();
        if (Track.getState()){
            imptk=(ImagePlus) WindowManager.getImage(name+TrackCB.getSelectedItem());

            //track ROIs
            if (imptk != null && Manager != null){
                Roi roi;
                imptk.setSlice(1);
                for (int CurIdx = 0; CurIdx < nRoi; CurIdx++){
                    roi=Manager.getRoi(CurIdx);
                    recenter(imptk, roi, 1);
                }
                for (int CurSlice = 1 ; CurSlice < MaxSlice ; CurSlice ++){
                    for (int CurIdx = 0; CurIdx < nRoi; CurIdx++){
                        roi=(Roi)rois_trkd.get(CurIdx+nRoi*(CurSlice-1));
                        recenter(imptk, roi, CurSlice+1);
                    }
                }
            }
        } else{
            rois_trkd = new Vector<Roi>();
            for (int CurSlice = 0 ; CurSlice < MaxSlice ; CurSlice ++){
                for (int CurIdx = 0; CurIdx < nRoi; CurIdx++){
                    Roi roi=Manager.getRoi(CurIdx);
                    rois_trkd.add(roi);
                
                }
            }
        }

        //read intensity
        ArrayList Intensities = new ArrayList<Vector>(ch);
        for (int c=0;c<ch;c++){
            ImageStatistics stat = new ImageStatistics();
            ImagePlus imp = WindowManager.getImage(name+(String)chs.get(c));
            imp.unlock();
            Vector Intensity=new Vector();
            for (int CurSlice = 0 ; CurSlice < MaxSlice ; CurSlice ++){
                imp.setSlice(CurSlice+1);
                for (int CurIdx = 0; CurIdx < nRoi; CurIdx++){ 
                    imp.setRoi((Roi)rois_trkd.get(CurIdx+nRoi*(CurSlice)));
                    stat = imp.getStatistics(2); // MEAN = 2
                    double Int_m =  stat.mean ;
                    Intensity.add(Int_m);
                }
            }
            Intensities.add(Intensity);
        }

        // creates the output file & write data
        String trkd;
        try {
            if (Track.getState())
                trkd = "Tracked-"+TrackCB.getSelectedItem();
                else
                trkd = "NonTracked";
            File file = new File(DIR,name+trkd+".csv");
            file.createNewFile();
            // creates a FileWriter Object
            FileWriter writer = new FileWriter(file,true); 
            for (int c=0;c<ch;c++){
                for (int i=0;i<nRoi;i++){
                writer.write((String)chs.get(c)+"_"+(i+1));
                writer.write(",");
                }
            }
            writer.write("\n");
            for (int i=0;i<MaxSlice;i++){
                for (int c=0;c<ch;c++){
                    for (int j=0;j<nRoi;j++){
                        writer.write(Double.toString((double)((Vector)Intensities.get(c)).get(j+nRoi*i)));
                        writer.write(",");
                    }
                }
                writer.write("\n");
            }
            writer.flush();
            writer.close();
        } catch (Exception e){
                IJ.showMessage("Output file failed!");
        }
    }
    
  
    public double[] getAverageData() {
        return (double[]) Average.getY().clone();
    }

    public int[] getSelectedIndexes() {
       int[] indexes = Manager.getSelectedIndexes();
       if (indexes==null || indexes.length==0)
           indexes = getAllIndexes();
       return indexes;
    }

   public int[] getAllIndexes(){
        int count = Manager.getCount();
        int[] indexes = new int[count];
        for (int i=0; i<count; i++)
           indexes[i] = i;
        return indexes;
    }

}

class TraceDataUC_Track_BTG extends Object{
    double[] xData = null;
    double[] yData = null;
    int CurrPos = 0;
    int DataLength = 0;
    //boolean Y_Only = false;

   public TraceDataUC_Track_BTG( int length){
        if (length > 0){
            DataLength = length;
            xData = new double[DataLength];
            yData = new double[DataLength];
        }
    }

   public TraceDataUC_Track_BTG( double[] x, double[] y){
        if( x != null && y != null){
            xData = (double[])x.clone();
            yData = (double[])y.clone();
            DataLength = Math.min(xData.length,yData.length);
        }
    }

   public boolean addData(double x, double y){
        if (CurrPos >= DataLength){
            IJ.showMessage("OOPS! I am full you can not add anymore to me");
            return false;
        }
        
        xData[CurrPos] = x;
        yData[CurrPos] = y;
        CurrPos++;
        return true;
    }

   public double getX(int pos){
       if(pos < DataLength)
           return xData[pos];
       return xData[DataLength];
   }

   public double getY(int pos){
      if(pos < DataLength) return yData[pos];
      return yData[DataLength];
   }

   public double[] getXY(int pos){
       double[] XY = new double[2];
       if (pos < DataLength){
            XY[1] = xData[pos];
            XY[2] = yData[pos];
       }
       else{
            XY[1] = xData[DataLength];
            XY[2] = yData[DataLength];
       }
       return XY;
   }

   public boolean setPosition(int pos){
       if(pos < DataLength){
           CurrPos = pos;
           return true;
       }
     return false;
   }

   public int getPosition(){
       return CurrPos;
   }

   public int getDataLength(){
       return DataLength;
   }

   public double[] getX(){
       return (double [])xData.clone();
   }

   public double[] getY(){
       return (double [])yData.clone();
   }

   public boolean setLength(int length){
       if (DataLength != 0)
           return false;
        if (length > 0){
            DataLength = length;
            xData = new double[DataLength];
            yData = new double[DataLength];
            return true;
        }
       return false;
   }

   public void OverrideLength(int length){
       if (length == 0){
           xData = null;
           yData = null;
           return;
       }
       DataLength = length;
       xData = new double[DataLength];
       yData = new double[DataLength];
       return; 
   }
}

}

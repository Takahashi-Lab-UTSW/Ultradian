/** 
 * Split channels, normalize stacks by rolling ball algorithm.
 * @author syang2
 */

import java.awt.*;
import java.util.*;
import ij.*;
import ij.ImagePlus;
import ij.io.OpenDialog;
import ij.io.Opener;
import ij.io.FileSaver;
import ij.process.*;
import ij.gui.*;
import ij.plugin.PlugIn;
import ij.plugin.filter.BackgroundSubtracter;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class UC_Substack implements PlugIn {
    static Frame Instance;
    Opener opener = new Opener();  
    public  String namedv, name;
    public  String KNP = new String ();
    public String dir;
    private int ch;
    private Vector chs = new Vector();
    
    public void run(String arg){ 
        open();
    }
    public void open(){

        ArrayList<JTextField> tfchs = new ArrayList<JTextField>();
        JPanel panel = new JPanel(new GridLayout(1,2,5,5));
        JFormattedTextField ftch = new JFormattedTextField(1);
        panel.add(new JLabel("Channel#:"));
        panel.add(ftch);

        int result = JOptionPane.showConfirmDialog(null, panel, 
               "Channels for Substacking:", JOptionPane.OK_CANCEL_OPTION);
        if (result == JOptionPane.OK_OPTION) {
            ch = ((Number)ftch.getValue()).intValue();
        }
        
        JPanel panel2 = new JPanel(new GridLayout(ch,2,5,5));
        for (int c=1;c<=ch;c+=1){
            JTextField tfch = new JTextField(10);
            panel2.add(new JLabel("Channel_"+c+" Name:"));
            panel2.add(tfch);
            tfchs.add(tfch);
        }
        int result2 = JOptionPane.showConfirmDialog(null, panel2, 
                   "Channels for Substacking:", JOptionPane.OK_CANCEL_OPTION);
        if (result2 == JOptionPane.OK_OPTION) {
            for (int c=1;c<=ch;c++){
                chs.addElement(tfchs.get(c-1).getText());
            }
        }
        
        OpenDialog od = new OpenDialog("Open dv file", null);  
        dir = od.getDirectory();
        String dv = od.getFileName();
        if (dv!=null) {
            String path = dir+dv;
            opener.open(path);
        }
        ImagePlus imp = WindowManager.getImage(dv);
        namedv = imp.getTitle();
        name=namedv.substring(0, namedv.length()-6);
        if (imp!=null){
            subStack(imp,chs);  // split channels
        }
        int n=0;
        for (int c=0; c<ch;c++){
            if (WindowManager.getImage(name+chs.get(c))!=null)
                n++;
        }
        if (n==ch)
            imp.close();
        subtractBackground();  // subtract background
        for (int c=0; c<ch;c++){
            WindowManager.getImage(name+chs.get(c)).show();
        }
    }
    
    public Vector getChs(){
        return chs;
    }
        
    public void subStack(ImagePlus imp, Vector chs){
        int Width = imp.getWidth();
        int Height = imp.getHeight();
        int nSliceDV = imp.getNSlices();
        int ch = chs.size();
        ArrayList<ImagePlus> imps = new ArrayList<ImagePlus>(ch);
        
        for (int c=1; c<=ch; c+=1){
            ImageStack stack = new ImageStack(Width, Height);
            ImageProcessor ip;
            for (int s=c; s<=nSliceDV; s+=ch){
                imp.setSlice(s);
                ip=imp.getProcessor(); //BeRST
                stack.addSlice(ip);
            }
            ImagePlus imp1 = new ImagePlus(name+chs.get(c-1),stack);
            new FileSaver(imp1).saveAsTiffStack(dir+name+chs.get(c-1));
            imps.add(imp1);
        }
        
        for (int c=1;c<=ch;c+=1){
            imps.get(c-1).show();
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
}


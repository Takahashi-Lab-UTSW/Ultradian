// install LOCI plugin to ImageJ;
// choose dir & get dv file names; Input channel number & names; Create a _split subfolder for each dv file to hold data;
// Split channels, subtract background by rolling ball 100, read intensity for the whole image; save intensity as csv file;

dir=getDirectory("Choose a Directory"); //print(dir); 
run("Set Measurements...", "mean integrated redirect=None decimal=4");
setBatchMode(true); 

dvfiles = newArray;
list = getFileList(dir);
for (i=0; i<list.length; i++) {
  if (endsWith(list[i], ".dv")){
    dvfiles=Array.concat(dvfiles,list[i]);
  }else 
    continue;
}

Dialog.create("Channel#");
Dialog.addNumber("Number of Cahnnels:", 2);
Dialog.show();
cn=Dialog.getNumber(); //channel number

if (cn != 0){
  cnm=newArray; //channel names
  Dialog.create("ChannelNames");
  for (i=0;i<cn;i++){
    Dialog.addString("Channel "+i+":", "name"+i);
  }
  Dialog.show();
  for (i=0;i<cn;i++){
    cnm=Array.concat(cnm,Dialog.getString());
  }
}

for (i=0; i<dvfiles.length; i++) { 

  fileName = dvfiles[i];                                 
  s=lastIndexOf(fileName, '.'); 
  ts=substring(fileName, 0, s); 
  splitDir=dir + "/" + ts + "_Split/"; 
  File.makeDirectory(splitDir); 
     
  run("Bio-Formats Importer", "open="+dir+fileName+" autoscale color_mode=Default split_channels view=Hyperstack stack_order=Default");
	//run("Bio-Formats Importer", "open="+dir+fileName+" autoscale color_mode=Default split_channels view=Hyperstack stack_order=XYCZT");
	
	for (j=0;j<cn;j++){
	  selectWindow(ts+".dv"+" - C="+j); 
  	saveAs("Tiff", splitDir+ts+"_"+cnm[j]+".tif");	
  	tg=getTitle(); s=lastIndexOf(tg, '.'); tgs=substring(tg, 0, s); 
  	run("Subtract Background...", "rolling=100 stack");
  	saveAs("Tiff", splitDir+tgs+"_n.tif");
  	tgn=getTitle(); s=lastIndexOf(tgn, '.'); tgns=substring(tgn, 0, s);
	  selectWindow(tgn);
	  
	  run("Clear Results");
		saveSettings;
	  setOption("Stack position", true);
	  for (n=1; n<=nSlices; n++) {
	     setSlice(n);
	     run("Measure");
	     }
	  restoreSettings;
	  updateResults();
	  saveAs("Results", splitDir+tgns+".csv"); 
	  run("Close");
	}
	
	//run("Merge Channels...", "c1="+tkn+" c2="+tgn+" create");
	//saveAs("Tiff", splitDir+ts+"merge.tif");
	run("Close All");

}

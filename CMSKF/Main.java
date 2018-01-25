import java.io.File;
import java.text.DecimalFormat;
import jxl.Cell;
import jxl.Sheet;
import jxl.Workbook;
import java.io.File;
import java.io.IOException;
import jxl.*;
import jxl.read.biff.BiffException;
import jxl.write.*;
import jxl.write.Number;
import jxl.write.biff.RowsExceededException;
import java.util.ArrayList;
import java.util.*;

 public class Main{
     
     
     
        public static void main(String[] args){
            
        String scenario="MSKF";
        int option=Integer.parseInt( args[0]);
        int IND=Integer.parseInt( args[1]);
        int REP=Integer.parseInt( args[2]);
        int START_F=Integer.parseInt(args[3]);
        int END_F=Integer.parseInt(args[4]);
        int top1=Integer.parseInt(args[5]);
        int LENGTH=Integer.parseInt( args[6]);
        int WEIGHT=Integer.parseInt(args[7]);
//        int IND=2;
//        int REP=2;
//        int START_F=16;
//        int END_F=17;
//        int top1=16;
//        int LENGTH=1;
//        int WEIGHT=1;
            
        int Ncs=0;
        int top;
        double[][][] Vars = new double[3][4][Ncs];
        double[][][] Dist = new double[6][4][Ncs];
        double[] Xhat = new double[Ncs];
        double ifault = 0;
        double[] Pie = { 0.9, 0.003, 0.003, 0.094 };

        String length = "temp_results";
            
            for(int ind=1;ind<IND;ind++){
                
             for(int rep=1;rep<REP;rep++){
                 
                for(int forecast_times=START_F;forecast_times<END_F;forecast_times++){
                        
                    
                            for(int jj=0;jj<LENGTH;jj++){

                            for(int weight=0;weight<WEIGHT;weight++){
                                
                                
                                String path_option="/Users/emiao/Desktop/sdforecast.xls";
                                
                                
                                int num_file=2;
//
//                                num_file=new File("/Users/emiao/Desktop/Model_selection/Synthetic/Exp_"+ind+"/Generated_data/replicate_"+rep+"/length"+jj+"/weight_0").listFiles().length;
                                for(int g=1;g<num_file;g++){

                                    try {
                                        
                                        Workbook book=Workbook.getWorkbook(new File("/Users/emiao/Desktop/Model_selection/Synthetic/Exp_"+ind+"/Generated_data/replicate_"+rep+"/timeseries.xls"));
                                        
                                        Sheet sheet = book.getSheet(0);
                                        
                                        String[][] original = new String[sheet.getRows()][sheet.getColumns()];
                                        
                                        for (int i = 0; i < sheet.getRows(); i++) {
                                            
                                            for (int j = 0; j < sheet.getColumns(); j++) {
                                                Cell cell = sheet.getCell(j, i);
                                                
                                                original[i][j] = cell.getContents();
                                                
                                            }
                                            
                                        }
                                        
                                        double[][] double_array = new double[sheet.getRows() - 1][sheet.getColumns()];
                                        
                                        
                                        top=top1+1+forecast_times;
                                        Ncs = sheet.getColumns();
                                        
                                        for (int i = 1; i < sheet.getRows(); i++) {
                                            
                                            for (int j = 0; j < sheet.getColumns(); j++) {
                                                
                                                double_array[i - 1][j] = Double.parseDouble(original[i][j]);
                                                
                                            }
                                            
                                        }
                                        
                                        Normalisation no=new Normalisation();
                                        Initialisation in=new Initialisation();
                                        BayesianMethod bm=new BayesianMethod();
                                        Results re=new Results();
                                        CMSKF cmskf=new CMSKF();
                                        WriteExcel we=new WriteExcel();
                                        
                                        int time = 0;
                                        double[][] PoolX = new double[sheet.getRows() - 1][sheet.getColumns()];
                                        double[] Vlaw = new double[Ncs - 1];
                                        double[][] CMSKF_pererr = new double[18][3];
                                        double[][] Yhat = new double[top][Ncs];
                                        time = 0;
                                        PoolX = no.normal(double_array, jj, top,  Ncs, top1);
                                        Vlaw = in.varianceLaw(PoolX,jj,Ncs,top1);
                                        Vars = in.initial_Vars(Vlaw, Ncs);
                                        Dist = in.initial_Dist(PoolX, Vars,Ncs);
                                        
                                        for (int t = jj; t < top1+1; t++){
                                            for (int j = 0; j < Ncs; j++) {
                                                Yhat[t][j] = PoolX[t][j];
                                            }
                                        }
                                        int s=1;
                                        
                                        if(scenario.equals("CMSKF")){
                                            
                                            if(Ncs==1){
                                                s=2;}else{s=1;}
                                            
                                        }else if(scenario.equals("MSKF")){
                                            
                                            
                                            s=2;}
                                        
                                        switch (s) {
                                            case 1:
                                                
                                                for (int t = jj + 1; t < top1+1; t++) {
                                                    
                                                    Dist = bm.bayesian(t, PoolX,Pie,Dist,Vars, top, Ncs);          /* call CMSKF algorithms */
                                                    Dist = cmskf.cross(t, PoolX,Dist,Vars,Ncs);
                                                    
                                                }
                                                Yhat=re.results(top1,Dist, Ncs,top,Yhat);
                                                
                                                
                                                break;
                                            case 2:
                                                
                                                for (int t = jj+1 ; t < top1+1; t++) {
                                                    Dist = bm.bayesian(t,PoolX,Pie,Dist,Vars,top,Ncs);            /* call MSKF algorithm */
                                                }
                                                
                                                Yhat= re.results(top1,Dist, Ncs,top,Yhat);
                                                break;
                                                
                                        }
                                        
                                        book.close();
                                        
                                        switch (option) {
                                            case 1:
                                                we.createbook( Ncs, forecast_times, path_option, top1, Yhat,  PoolX);
                                                
                                                break;
                                            case 2:
                                                we.appenddata( path_option,  Yhat,PoolX,  length, forecast_times,  Ncs, top1,g);
                                                break;
                                        }
                                        
                                    }catch (Exception e)
                                    
                                    {
                                        System.out.println(e);
                                    
                                      }
                                  }
                               
                           }
                        }
                    }
        
                 }
    
            }

        }
     
 }



     



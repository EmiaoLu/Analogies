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
//    double sum;
//    double sum1;
//    public static int Ncs;
//    public static int top;
//    public static int top1=16;
//    double[] Vlaw = new double[Ncs];
//    double[] Xhat = new double[Ncs];
//    double[][][] Vars = new double[3][4][Ncs];
//    double[][][] Dist = new double[6][4][Ncs];
//    double ifault = 0;
//    static double extreme = 9999.0;
//    int No=Integer.parseInt( args[0]);
//    int rep=Integer.parseInt( args[1]);
//    int number=Integer.parseInt(args[2]);
//    int jj=Integer.parseInt( args[3]);
//    int weight=Integer.parseInt(ar


 public class Main extends Thread{
     
    private void run(int ind){
   
        String scenario="MSKF";
        int option=2;
        int top;
        double sum;
        double sum1;
        int Ncs=0;

        double[] Xhat = new double[Ncs];
        double[][][] Vars = new double[3][4][Ncs];
        double[][][] Dist = new double[6][4][Ncs];
        double ifault = 0;
        double extreme = 9999.0;
        double[] Pie = { 0.9, 0.003, 0.003, 0.094 };

        String length = "temp_results";
        
        String method="foo";
        
//        for(int h=15;h<16;h++){
        
            for(int rep=1;rep<31;rep++){
                
                
//                    for(int sap=1;sap<51;sap++){
                
                    for(int forecast_times=6;forecast_times<7;forecast_times++){
                        
                        for(int top1=16;top1<17;top1++){

                      int   horizon=top1+forecast_times+1;
                        
                        
                            for(int jj=0;jj<6;jj++){

                            for(int weight=0;weight<1;weight++){
                                
                                
                                String path_option="/Users/emiao/Desktop/Model_selection/Synthetic/Exp_"+ind+"/MSKF/Forecast_23/rep_"+rep+"/length"+jj+"/forecast.xls";
                                
                                
                                int num_file=2;
//
//                                num_file=new File("/Users/emiao/Desktop/Model_selection/Synthetic/Exp_"+ind+"/Generated_data/replicate_"+rep+"/length"+jj+"/weight_0").listFiles().length;
                                System.out.println("num_file"+num_file);
                                for(int g=1;g<num_file;g++){

                                    try {
                                        
                                        Workbook book=Workbook.getWorkbook(new File("/Users/emiao/Desktop/Model_selection/Synthetic/Exp_"+ind+"/Generated_data/replicate_"+rep+"/timeseries.xls"));

                                        
                                        Sheet sheet = book.getSheet(0);
                                        
                                        String[][] original = new String[sheet.getRows()][sheet.getColumns()];
                                        
                                        for (int i = 0; i < sheet.getRows(); i++) {
                                            
                                            for (int j = 0; j < sheet.getColumns(); j++) {
                                                Cell cell = sheet.getCell(j, i);
                                                
                                                original[i][j] = cell.getContents();
                                                
                                                // System.out.print(original[i][j] + " ");
                                                
                                            }
                                            
                                            //System.out.println();
                                            
                                        }
                                        
                                        double[][] double_array = new double[sheet.getRows() - 1][sheet.getColumns()];
                                        
                                        //top = sheet.getRows() - 1;     ///the historical+forecasting
                                        
                                        top=top1+1+forecast_times;
                                        //top=14;
                                        Ncs = sheet.getColumns();
                                        
                                        for (int i = 1; i < sheet.getRows(); i++) {
                                            
                                            for (int j = 0; j < sheet.getColumns(); j++) {
                                                
                                                double_array[i - 1][j] = Double.parseDouble(original[i][j]);
                                                //System.out.print(double_array[i-1][j] + " ");
                                                
                                                
                                            }
                                            // System.out.println();
                                            
                                        }
                                        
                                        //                                        Main ob = new Main();
                                        Normalisation no=new Normalisation();
                                        Initialisation in=new Initialisation();
                                        BayesianMethod bm=new BayesianMethod();
                                        Results re=new Results();
                                        CMSKF cmskf=new CMSKF();
                                        WriteExcel we=new WriteExcel();
                                        NaiveMethod nm=new NaiveMethod();
                                        
                                        int time = 0;
                                        double[][] PoolX = new double[sheet.getRows() - 1][sheet.getColumns()];
                                        double[] Vlaw = new double[Ncs - 1];
                                        //                                        double[][][] Vars;
                                        //                                        double[][][] Dist;
                                        //                                        double[][] MSKF_pererr = new double[18][3];
                                        //                                        double[][] CMSKF_pererr = new double[18][3];
                                        double[][] Yhat = new double[top][Ncs];
                                        //                                        double MSKF = 0.0;
                                        //                                        double CMSKF = 0.0;
                                        //                                        double[][] Yhat_Naive = new double[top][Ncs];
                                        //                                        double[][] Naive_drift = new double[top][Ncs];
                                        //                                        double[] drift_err = new double[3];
                                        
                                        
                                        
                                        time = 0;
                                        
                                        PoolX = no.normal(double_array, jj, top,  Ncs, top1, extreme);
                                        
                                        Vlaw = in.varianceLaw(PoolX,jj,Ncs,top1,extreme);
                                        
                                        Vars = in.initial_Vars(Vlaw, Ncs);
                                        Dist = in.initial_Dist(PoolX, Vars,Ncs,extreme);
                                        
                                        
                                        
                                        for (int t = jj; t < top1+1; t++){
                                            for (int j = 0; j < Ncs; j++) {
                                                Yhat[t][j] = PoolX[t][j];
                                                ///System.out.println("Prediction:"+PoolX[t][j]);
                                            }//System.out.println();
                                        }
                                        int s=1;
                                        
                                        if(scenario.equals("CMSKF")){
                                            
                                            if(Ncs==1){
                                                s=2;}else{s=1;}
                                            
                                        }else if(scenario.equals("MSKF")){
                                            
                                            
                                            s=2;}
                                        
                                        switch (s) {
                                            case 1: ///C-MSKF
                                                
                                                for (int t = jj + 1; t < top1+1; t++) {
                                                    
                                                    Dist = bm.bayesian(t, PoolX,Pie,Dist,Vars, top, Ncs, extreme);
                                                    
                                                    Dist = cmskf.cross(t, PoolX,Dist,Vars,Ncs,extreme);
                                                    
                                                }
                                                Yhat=re.results(top1,Dist, Ncs,top,Yhat, extreme);
                                                
                                                
                                                break;
                                            case 2:
                                                
                                                for (int t = jj+1 ; t < top1+1; t++) {
                                                    
                                                    Dist = bm.bayesian(t,PoolX,Pie,Dist,Vars,top,Ncs,extreme);
                                                }
                                                
                                                
                                                
                                                Yhat= re.results(top1,Dist, Ncs,top,Yhat,extreme);
                                                
                                                break;
                                                
                                                
                                        }
                                        
                                        
                                        book.close();
                                        
                                        
                                        
                                        // //////////////////////////////////Option for producing
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
//        }
    }
     
     
      public static void main(String[] args){
//        Main obj1=new Main();
//        obj1.run(1);
//        obj1.start();
//
//        Main obj2=new Main();
//        obj2.run(2);
//        obj2.start();

        Main obj5=new Main();
        obj5.run(4);
        obj5.start();
    
      }
        
 }
     



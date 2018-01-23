import java.io.File;
import java.text.DecimalFormat;
import jxl.Cell;
import jxl.Sheet;
import jxl.Workbook;
import java.io.File;
import java.io.IOException;
import jxl.*;
import jxl.write.*;
import jxl.write.Number;
import jxl.write.biff.RowsExceededException;
import java.util.ArrayList;



public class WriteExcel{
    


    
    protected void createbook(int Ncs,int forecast_times,String path_option,int top1,double[][] Yhat, double[][] PoolX ) {
      
        try {

            double[] num_Yhat=new double[Ncs*forecast_times];
            double[] num_Original=new double[Ncs*forecast_times];
            
            WritableWorkbook workbook = Workbook.createWorkbook(new File(path_option));

            WritableSheet sheet = workbook.createSheet("temp_results", 2);
            Label label = new Label(0, 0, "Original");
            Label label1=new Label(1,0,"Forecast");
            sheet.addCell(label);
            sheet.addCell(label1);
            
            workbook.write();
            workbook.close();
        
            
        } catch (Exception e)
        
        {
            
            System.out.println(e);
            
        }
    }
    
    
    
///void appenddata(String path_option, double[][] Yhat,double[][] PoolX, String length,int forecast_times, int Ncs,int top1)
  protected void appenddata(String path_option, double[][] Yhat,double[][] PoolX, String length,int forecast_times, int Ncs,int top1,int g) {
{
    
        try {
            double[] num_Yhat=new double[Ncs*forecast_times];
            double[] num_Original=new double[Ncs*forecast_times];
            int No=Ncs*forecast_times;
            
            Workbook existingbook = Workbook.getWorkbook(new File(path_option));
            WritableWorkbook copy = Workbook.createWorkbook(new File(path_option), existingbook);
            WritableSheet sheet = copy.getSheet(length);// //individual
            // sheet;
            
            
            int row;
            int col;
            
            row=sheet.getRows();

      
          //  col=sheet.getColumns();
           // System.out.println("row "+row);
            existingbook.close();
            
           

            int k=0;

            
            for (int j = 0; j <Ncs; j++) {

            for (int i = top1+1; i < top1+1+forecast_times; i++) {
                num_Original[k]=PoolX[i][j];
                num_Yhat[k] = Yhat[i][j];

                    k++;
                    
                }
               // System.out.println("number"+Ncs);
                
                
            }
            
            
            for (int j = 0; j < Ncs*forecast_times; j++) {
          
                
                    Number num1 = new Number( 0, j+row, num_Original[j]);
                  Number num2 = new Number( 1, j+row, num_Yhat[j]);
                
                
                // System.out.println(number[j]);
                
                sheet.addCell(num1);
                sheet.addCell(num2);
                
            }
            
            
            
            
            copy.write();
            copy.close();
            
        } catch (Exception e)
        
        {
            
            System.out.println(e);
            
        }
}}
    
  
}


    
    
    
    
    
    
    
    
    
    
    
    
    

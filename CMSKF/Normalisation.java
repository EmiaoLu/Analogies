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



public class Normalisation{
    public double sum = 0.0;
    public  double sum1=0.0;
    int top2;
    
    
    
    protected double[][] normal(double[][] PoolX, int jj,int top, int Ncs,int top1,double extreme) {
        
        double[][] normal_PoolX=new double[top][Ncs];
        
        //System.out.println("top1="+top2);
        for (int j = 0; j < Ncs; j++) {
            top2=top1;
            
            
            for (int i = jj; i < top; i++) {
                
                normal_PoolX[i][j] = PoolX[i][j];
                //System.out.println(normal_PoolX[i][j]);
            }
            
            sum = 0.0;
            for (int i = jj; i < top1 + 1; i++) {
                
                if(normal_PoolX[i][j]!=extreme){
                    
                    sum=sum+normal_PoolX[i][j];
                    
                }else{
                    top2--;
                }
                
                
                
            }
            //System.out.println("top2="+top2);
            
            sum = sum / (top2 - jj + 1);
            //System.out.println("sum="+sum);
            
            sum1 = 0.0;
            
            for (int i = jj; i < top1 + 1; i++) {
                
                if(normal_PoolX[i][j]!=extreme){
                    
                    sum1 = sum1 + Math.pow((normal_PoolX[i][j] - sum), 2);
                }
                
            }
            
            sum1 = sum1 / (top2 - jj + 1);
            
            if(sum1==0){
                for (int i = jj; i < top; i++) {
                    if (normal_PoolX[i][j] != extreme) {
                        
                        normal_PoolX[i][j] = (normal_PoolX[i][j] - sum);
                        
                    }
                }
                
                
            }
            
            else{
                
                for (int i = jj; i < top; i++) {
                    if (normal_PoolX[i][j] != extreme) {
                        
                        normal_PoolX[i][j] = (normal_PoolX[i][j] - sum) / Math.sqrt(sum1)+10;
                       System.out.println(normal_PoolX[i][j]);
                        
                    }
                }
            }
            
            
            
            
            
            
            // System.out.println("Individual Time Series: ");
            for (int i = jj; i < top; i++) {
                if(normal_PoolX[i][j]<0){
                   // System.out.println("report a negative number of the scaled time series! "+normal_PoolX[i][j]);
                    
                }else{
                   // System.out.println("scaled time series "+normal_PoolX[i][j]);
                }
            }
           //System.out.println();
            
            int count=0;
            
            for(int i=jj;i<top;i++){
                
                
                if(Double.isNaN(normal_PoolX[i][j])||Double.isInfinite(normal_PoolX[i][j])){
                   // System.out.println("NaN or Infinity is presented in scaled time series!");
                    System.exit(1);
                    
                }
                
                //                if(normal_PoolX[i][j]!=extreme){
                //                    count++;
                //
                //                }
                //                if(count==0){
                //                    System.out.println("Report a empty time series!");
                //                    System.exit(1);
                //                
                //                }
                
            }
        }
        return normal_PoolX;
        
    }
    
    
}

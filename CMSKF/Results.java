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



public class Results{
    
    protected double[][] results(int top1, double[][][] Dist,int Ncs,int top,double[][] Yhat, double extreme) {
        
        double BigT;
        double BigS;
        int count1;
        int count2;
       
        for(int j=0;j<Ncs;j++){
            BigT = 0.0;
            BigS = 0.0;
            
            for (int ii = 0; ii < 4; ii++) {
                BigT = BigT + Dist[5][ii][j] * Dist[0][ii][j];
                
                BigS = BigS + Dist[5][ii][j] * Dist[1][ii][j];
                
            }
            count1=2;
            count2=1;

            
            for(int t=top1+1;t<top;t++){
                

                
                if(Yhat[top1][j]==extreme){
                    
                    Yhat[t][j]=BigT+count1*BigS;
                  //  System.out.println("  missing "+count1+ " pred "+Yhat[t][j]);

                    count1=count1+1;
                    
                    
                }else{
                    Yhat[t][j]=BigT+count2*BigS;
                     //System.out.println(" without missing "+count2+ " pred "+Yhat[t][j]);
                    count2=count2+1;
                    
                    

                }

                
            }
            
        }
        return Yhat;
        
        
    }
    
    
}


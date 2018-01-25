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



public class Initialisation{
    
   protected double[] varianceLaw(double[][] PoolX, int jj,int Ncs,int top1,double extreme) {
        
        Median me=new Median();
        double[] Vlaw = new double[Ncs];
        int nine;
        int ten;
        int k;
        double med1=0.0;
        double med2;
        int min = 10;
        int count ;
        double sum;
        
        
        for (int j = 0; j <Ncs; j++) {
            
            
            count=0;
            
            for (int i = jj; i < top1+1; i++) {
                
                if (PoolX[i][j]!= extreme) {
                    
                    count++;
                    
                }
            }
           ten=Math.min(min,count);
            
            nine=ten-1;
            
            count=0;
            sum=0.0;
            
            k=jj;
            
            while(count<nine){
                
                
                    sum=sum+PoolX[k+1][j];
                
                
                count++;
                k++;
                
            }
          
            sum = sum / nine;


            
            double[] Buf1 = new double[nine];
            
            
            for (int i = 0; i < nine; i++) {
                Buf1[i] = 0;
                
            }
            
            count=0;
            k=jj;
            
            while(count<nine) {
                
                    Buf1[count] = Math.abs(PoolX[k+1][j] - sum);
                    count++;
                
                k++;
            }
            
            
            
            
                med1 = me.getMedian(Buf1,nine);
            
            count=0;
            
            for (int i = jj; i < top1; i++) {
                
                
                    count++;
            }
            
           ten=Math.min(min,count);
            
            nine=ten-1;
            
            double[] Buf2 = new double[nine];
            
            for (int i = 0; i < nine; i++) {
                Buf2[i] = 0.0;
                
            }
            
            count=0;
            k=jj;

            while(count<nine) {
                
                    Buf2[count] = Math.abs(PoolX[k+1][j] - PoolX[k][j]);

                    count++;
                k++;
                
            }
            
            med2 = me.getMedian(Buf2, nine);
 
            Vlaw[j] = med1;
            
            if (med1 > med2){
                Vlaw[j] = med2;
                
            }else{

            }
            
            
            Vlaw[j] = (Vlaw[j] / 0.6745) * (Vlaw[j] / 0.6745);

       }
        
        
        
              sum = 0.0;
        for (int j = 0; j < Ncs; j++)
            sum = sum + Vlaw[j];
        sum = sum / Ncs;
        for (int j = 0; j < Ncs; j++) {
            Vlaw[j] = sum * 10;

        }
        return Vlaw;
    }
    
    protected double[][][] initial_Vars(double[] Vlaw,int Ncs) {
        double[][][] Vars = new double[3][4][Ncs];
        
        double[] Rg = { 0.0, 100.0, 0.0, 0.0 };
        double[] Rr = { 10.0, 0.0, 1.0, 0.0 };
        double[] Re = { 1.0, 1.0, 1.0, 101.0 };
        
        for (int j = 0; j < Ncs; j++) {
            for (int i = 0; i < 4; i++) {
                Vars[0][i][j] = Re[i] * Vlaw[j];
                Vars[1][i][j] = Rg[i] * Vlaw[j];
                Vars[2][i][j] = Rr[i] * Vlaw[j];
            }

        }
        
        
        return Vars;
    }
    
    double[][][] initial_Dist(double[][] PoolX, double[][][] Vars,int Ncs) {
        
        double[][][] Dist = new double[6][4][Ncs];
        
        int count;
        int ind;
        
        
        for (int j = 0; j < Ncs; j++) {
            
            
            for (int i = 0; i < 4; i++) {
                count=1;
                ind=0;
                
                while(count<2){
                    
                    
                        Dist[0][i][j] = PoolX[ind][j];
                        
                        count++;
                        
                    ind++;
                    
                }
                
                count=1;
                ind=0;
                
                while(count<2){
                    
                    
                        Dist[1][i][j] = PoolX[ind+1][j]- PoolX[ind][j];
                        count++;
                        
                    ind++;
                }
                
                Dist[2][i][j] = Vars[2][i][j] + Vars[1][i][j];
                Dist[3][i][j] = Vars[2][i][j];
                Dist[4][i][j] = Vars[2][i][j];
                Dist[5][i][j] = 0.25;
                
            }
        }
        return Dist;
        
    }
    
}

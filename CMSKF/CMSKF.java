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

public class CMSKF{
    
    DigammaMethod di=new DigammaMethod();
    protected double[][][] cross(int t, double[][] PoolX, double[][][] Dist,double[][][] Vars,int Ncs) {
        double sum1;
        double sum2;
        double sum3;
        double sum4;
        double Const1;
        double Const2;
        double ii;
        double x1;
        double x2;
        double avrX;
        double Value;
        double NiuT;
        double NiuS;
        double SoT;
        double SoS;
        double SMofT;
        double SVofT;
        double SMofS;
        double SVofS;
        int Ncs_T=0;
        int Ncs_S=0;
        
        for (int i = 0; i < 4; i++) {
            sum1 = 0.0;
            sum2 = 0.0;
            sum3 = 0.0;
            sum4 = 0.0;
            
            Ncs_T=0;
            Ncs_S=0;
            
            
            for (int jj = 0; jj < Ncs; jj++) {
                
                    
                    
                    sum1 = sum1 + 1.0 / Dist[2][i][jj];
                
                    sum2 = sum2 + Math.log(Dist[2][i][jj]);
                    
                    Ncs_T++;
                
                
                
                    sum3 = sum3 + 1.0 / Dist[4][i][jj];
                    sum4 = sum4 + Math.log(Dist[4][i][jj]);
                
                    Ncs_S++;

                
                
                
            }
            
            
            Const1 = 0.5 * (Math.log(Ncs_T) - Math.log(sum1) - sum2 / Ncs_T);
            Const2 = 0.5 * (Math.log(Ncs_S) - Math.log(sum3) - sum4 / Ncs_S);
            ii = 0;
            x1 = 0.001;
            x2 = 50.0;
            do {
                avrX = (x1 + x2) / 2;
                Value = di.digamma(avrX, 1) - 0.5 * Math.log(avrX) - Const1;
                
                ii = ii + 1;
                if (Value > 0.0)
                    x2 = avrX;
                else
                    x1 = avrX;
            }
            
            while ((Value > 0.01 || Value < -0.01) && ii < 20);
            NiuT = avrX * 2;
            if (NiuT < 2.0)
                NiuT = 2;
            SoT = (NiuT * Ncs_T) / sum1;
            ii = 0;
            x1 = 0.001;
            x2 = 50.0;
            do {
                avrX = (x1 + x2) / 2;
                Value = di.digamma(avrX, 1) - 0.5 * Math.log(avrX) - Const2;
                ii = ii + 1;
                if (Value > 0)
                    x2 = avrX;
                else
                    x1 = avrX;
            } while ((Value > 0.01 || Value < -0.01) && ii < 20);
            
            NiuS = avrX * 2;
            if (NiuS < 2.0)
                NiuS = 2.0;
            SoS = (NiuS * Ncs_S) / sum3;
            
            for (int j = 0; j < Ncs_T; j++) {
                
                
                    Dist[2][i][j] = (SoT + Math.pow((PoolX[t - 1][j] - Dist[0][i][j]), 2) / (NiuT - 1));
                    
                    
                
                
            }
            for (int j=0;j<Ncs_S;j++){
                
                    Dist[4][i][j] = (SoS + Math.pow((PoolX[t][j] - PoolX[t - 1][j])- Dist[1][i][j], 2))/ (NiuS - 1);
            }
            
        }
        for (int i = 0; i <4 ; i++) {
            SMofT = 0.0;
            SVofT = 0.0;
            SMofS = 0.0;
            SVofS = 0.0;
            for (int jj = 0; jj < Ncs_T; jj++) {
                SMofT = SMofT + Dist[0][i][jj];
                
            }
            for (int jj = 0; jj < Ncs_S; jj++) {
                SMofS = SMofS + Dist[1][i][jj];
                
            }
            SMofT = SMofT / Ncs_T;
            SMofS = SMofS / Ncs_S;
            
            for (int jj = 0; jj < Ncs_T; jj++) {
                SVofT = SVofT + Math.pow((Dist[0][i][jj] - SMofT), 2);
                
            }
            for (int jj = 0; jj < Ncs_S; jj++) {
                SVofS = SVofS + Math.pow((Dist[1][i][jj] - SMofS), 2);
                
            }
            
            
            
            SVofT = SVofT / Ncs_T;
            SVofS = SVofS / Ncs_S;
            
            for (int j = 0; j < Ncs_T; j++) {
                    Dist[0][i][j] = (Dist[2][i][j] * SMofT + PoolX[t - 1][j]* SVofT)/ (Dist[2][i][j] + SVofT);
           
            }
            
            for (int j = 0; j < Ncs_S; j++) {
                
                    Dist[1][i][j] = (Dist[4][i][j] * SMofS + (PoolX[t][j] - PoolX[t - 1][j])* SVofS)/ (Dist[4][i][j] + SVofS);
                
            }
            System.out.println();
            
        }
        return Dist;
        
    }
    
}

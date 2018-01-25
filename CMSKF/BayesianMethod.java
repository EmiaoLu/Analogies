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


public class BayesianMethod{
    
    protected double[][][] bayesian(int t, double[][] PoolX, double[] Pie,double[][][] Dist, double[][][] Vars,int top,int Ncs ) {
        double[][] Mi = new double[4][top];
        double[][] Bi = new double[4][top];
        double[][] Vmm = new double[4][top];
        double[][] Vmb = new double[4][top];
        double[][] Vbb = new double[4][top];
        double[][] Q = new double[4][top];
        double Mf[][][] = new double[4][4][top];
        double Bf[][][] = new double[4][4][top];
        double Vmmf[][][] = new double[4][4][top];
        double Vmbf[][][] = new double[4][4][top];
        double Vbbf[][][] = new double[4][4][top];
        double[] Vet = new double[4];
        double[] Vg = new double[4];
        double[] Vr = new double[4];
        double[][] Ei = new double[4][top];
        double r11 = 0;
        double r12 = 0;
        double r22 = 0;
        double Ve[][] = new double[4][4];
        double A1 = 0;
        double A2 = 0;
        double invK;
        double arg = 0;
        double arg2 = 0;
        double P[][][] = new double[4][4][top];
        double arg1;
        
        for (int jo = 0; jo < Ncs; jo++) {
            
                for (int i = 0; i < 4; i++) {
                    
                    Mi[i][t - 1] = Dist[0][i][jo];
                    Bi[i][t - 1] = Dist[1][i][jo];
                    Vmm[i][t - 1] = Dist[2][i][jo];
                    Vmb[i][t - 1] = Dist[3][i][jo];
                    Vbb[i][t - 1] = Dist[4][i][jo];
                    Q[i][t - 1] = Dist[5][i][jo];
                    Vet[i] = Vars[0][i][jo];
                    Vg[i] = Vars[1][i][jo];
                    Vr[i] = Vars[2][i][jo];
                }
            
                for (int i = 0; i < 4; i++){
                    
                    for (int j = 0; j < 4; j++) {
                        
                        Ei[i][t] = PoolX[t][jo] - Mi[i][t - 1] - Bi[i][t - 1];
                        
                        r11 = Vmm[i][t - 1] + 2 * Vmb[i][t - 1] + Vbb[i][t - 1]+ Vg[j] + Vr[j];
                        r12 = Vmb[i][t - 1] + Vbb[i][t - 1] + Vr[j];
                        r22 = Vbb[i][t - 1] + Vr[j];
                        Ve[i][j] = r11 + Vet[j];
                        A1 = r11 / Ve[i][j];
                        A2 = r12 / Ve[i][j];
                        
                        Mf[i][j][t] = Mi[i][t - 1] + Bi[i][t - 1] + A1 * Ei[i][t];
                        
                        
                        Bf[i][j][t] = Bi[i][t - 1] + A2 * Ei[i][t];
                        Vmmf[i][j][t] = r11 - A1 * A1 * Ve[i][j];
                        if (Vmmf[i][j][t] < 0.0)
                            Vmmf[i][j][t] = 0.0;
                        Vmbf[i][j][t] = r12 - A1 * A1 * Ve[i][j];
                        if (Vmbf[i][j][t] < 0.0)
                            Vmbf[i][j][t] = 0.0;
                        Vbbf[i][j][t] = r22 - A2 * A2 * Ve[i][j];
                        if (Vbbf[i][j][t] < 0.0)
                            Vbbf[i][j][t] = 0.0;
                        
                    }
                }
                invK = 0;
                for (int ii = 0; ii < 4; ii++){
                    for (int jj = 0; jj < 4; jj++) {
                        arg = -Math.pow((PoolX[t][jo] - Mi[ii][t - 1] - Bi[ii][t - 1]), 2)/ (2 * Ve[ii][jj]);
                        arg2 = 2 * 3.14159265 * Ve[ii][jj];
                        invK = invK + Q[ii][t - 1] * Pie[jj] * Math.pow(Math.E, arg) / Math.sqrt(arg2);
                    }
                }
            
                for (int i = 0; i < 4; i++){
                    for (int j = 0; j < 4; j++) {
                        arg = -Math.pow((PoolX[t][jo] - Mi[i][t - 1] - Bi[i][t - 1]), 2)/ (2 * Ve[i][j]);
                        arg2 = 2 * 3.14 * Ve[i][j];
                        P[i][j][t] = Q[i][t - 1] * Pie[j] * Math.pow(Math.E, arg)/ (Math.sqrt(arg2) * invK);
                        
                    }
                }
                
                for (int j = 0; j < 4; j++) {
                    Q[j][t] = 0.0;
                    Bi[j][t] = 0.0;
                    Mi[j][t] = 0.0;
                    Vmm[j][t] = 0.0;
                    Vmb[j][t] = 0.0;
                    Vbb[j][t] = 0.0;
                    for (int i = 0; i < 4; i++) {
                        Q[j][t] = Q[j][t] + P[i][j][t];
                    }
                    for (int i = 0; i < 4; i++) {
                        Bi[j][t] = Bi[j][t] + (P[i][j][t] * Bf[i][j][t]) / Q[j][t];
                        Mi[j][t] = Mi[j][t] + (P[i][j][t] * Mf[i][j][t]) / Q[j][t];
                    }
                    for (int i = 0; i < 4; i++) {
                        arg1 = Mf[i][j][t] - Mi[j][t];
                        arg2 = Bf[i][j][t] - Bi[j][t];
                        Vmm[j][t] = Vmm[j][t] + P[i][j][t]* (Vmmf[i][j][t] + arg1 * arg1) / Q[j][t];
                        Vmb[j][t] = Vmb[j][t] + P[i][j][t]* (Vmbf[i][j][t] + arg1 * arg2) / Q[j][t];
                        Vbb[j][t] = Vbb[j][t] + P[i][j][t] * (Vbbf[i][j][t] + arg2 * arg2) / Q[j][t];
                        
                    }
                    for (int i = 0; i < 4; i++) {
                        Dist[0][i][jo] = Mi[i][t];
                        Dist[1][i][jo] = Bi[i][t];
                        Dist[2][i][jo] = Vmm[i][t];
                        Dist[3][i][jo] = Vmb[i][t];
                        Dist[4][i][jo] = Vbb[i][t];
                        Dist[5][i][jo] = Q[i][t];
                        
                    }
                }
                
            }
        return Dist;
        
    }
    

    
}

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



public class DigammaMethod{
    
   protected double digamma(double x, double ifault) {
        double digamma;
        double y;
        double r;
        double S = 0.00001;
        double C = 8.5;
        double S3 = 0.08333333333;
        double S4 = 0.008333333333;
        double S5 = 0.003968253968;
        double d1 = -0.5772156649;
        
        digamma = 0.0;
        y = x;
        ifault = 1;
        
        if (y <= 0.0) {
            ifault = 0.0;
            return ifault;
        }else{
        
        if (y <=S) {
            digamma = d1 - 1.0 / y;
            
            return digamma;
        }else{
        
        while (y <C) {
            
            digamma = digamma - 1.0 / y;
            y = y + 1.0;
            
        }
        r = 1.0 / y;
        digamma = digamma + Math.log(y) - 0.5 * r;
        r = r * r;
        digamma = digamma - r* (S3 - r * (S4-r*S5));
        return digamma;
        }
    }
}
}

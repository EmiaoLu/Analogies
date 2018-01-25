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



public class Median{
    
    
    
    protected double getMedian(double a[], int n) {
        
        double maxim;
        double s[] = new double[n];
        double getmedian;
        int ind;
        
        
        for (int i = 0; i < n; i++) {
            ind = 0;
            maxim = a[0];
            for (int ii = 0; ii < n; ii++) {
                if (a[ii] > maxim) {
                    ind = ii;
                    maxim = a[ii];
                }
                
            }
            
            
            s[i] = maxim;
            a[ind] = -1;
        }
        
        
        ind = (int) (n / 2);
        getmedian = s[ind];
        if ((n / 2) * 2 == n)
            getmedian = (s[ind-1] + s[ind]) / 2;
        return getmedian;
    }
    

    
    
}

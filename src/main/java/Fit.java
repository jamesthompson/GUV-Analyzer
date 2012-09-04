package com.jamesrthompson.Fitting;

import java.util.Iterator;
import java.util.List;
import javafx.scene.chart.XYChart;

/**
 * Abstract base class for all fitting classes
 * @author James R. Thompson, D.Phil
 * @since May 30, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. &amp; Mat. Sci. - University of Southern California
 */

public abstract class Fit {

    protected XYChart.Series<Number, Number> seriesToFit = null;
    protected XYChart.Series<Number, Number> theFit = null;
    
    /**
     * Init method
     * @param seriesToFit - The data series to fit stored as a field in the subclass object
     */
    public void fitInit(XYChart.Series<Number, Number> seriesToFit) {
        this.seriesToFit = seriesToFit;
    }
    
    /**
     * Init method
     * @param yvalues - The y-values for the data - a List<? extends Number> object
     * @param xvalues - The x-values for the data - a List<? extends Number> object
     */
    public void fitInit(List<? extends Number> yvalues, List<? extends Number> xvalues) {
        seriesToFit = new XYChart.Series<>();
        for (Iterator<? extends Number> it = yvalues.iterator(); it.hasNext();) {
            Number val = it.next();
            seriesToFit.getData().add(new XYChart.Data<>(val, xvalues.get(yvalues.indexOf(val))));
        }
    }
    
    /**
     * Init method
     * @param yvalues - The y-values for the data - a double[]
     * @param xvalues - The x-values for the data - a double[]
     */
    public void fitInit(double[] yvalues, double[] xvalues) {
        seriesToFit = new XYChart.Series<>();
        for(int i = 0; i < yvalues.length; i++) {
            seriesToFit.getData().add(new XYChart.Data<Number, Number>(yvalues[i],xvalues[i])); // Must let compiler know we're giving it Numbers...? Or do we..?
        }
    }
    
    /**
     * Get the data series used for fitting.
     * @return - Data series of the raw data.
     */
    public XYChart.Series<Number, Number> returnRawData() {
        return seriesToFit;
    }
    
    /**
     * Get a fit of the data series that was used. (n.b. The fit will have the same number of points as the original data).
     * @return - Data series of the fit.
     */
    public XYChart.Series<Number, Number> returnFitData() {
        return seriesToFit;
    }
    
    /**
     * Get the fitting residuals.
     * @return - Data series of the fitting residuals.
     */
    public XYChart.Series<Number, Number> getResiduals() {
        XYChart.Series<Number, Number> residuals = new XYChart.Series<>();
        for(XYChart.Data<Number, Number> point : seriesToFit.getData()) {
            double resid = point.getYValue().doubleValue() - theFit.getData().get(seriesToFit.getData().indexOf(point)).getYValue().doubleValue();
            residuals.getData().add(new XYChart.Data<Number, Number>(point.getXValue(), resid));
        }
        return residuals;
    }
    
}

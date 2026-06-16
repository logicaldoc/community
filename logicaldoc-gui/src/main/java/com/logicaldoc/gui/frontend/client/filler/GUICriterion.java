package com.logicaldoc.gui.frontend.client.filler;

import java.io.Serializable;

/**
 * A criterion for rating labels processed by an Attribute Filler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class GUICriterion implements Serializable {

    private static final long serialVersionUID = 1L;

    private String operator = "INCLUDED";

    private String operand;

    private Double metric;

    public GUICriterion() {
        // Empty criterion
    }
    
    public GUICriterion(String operator, String operand, Double metric) {
        super();
        this.operator = operator;
        this.operand = operand;
        this.metric = metric;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public String getOperand() {
        return operand;
    }

    public void setOperand(String operand) {
        this.operand = operand;
    }

    public Double getMetric() {
        return metric;
    }

    public void setMetric(Double metric) {
        this.metric = metric;
    }
    
    
    
}
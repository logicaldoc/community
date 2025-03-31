package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;
import java.util.Date;

/**
 * The result of an evaluation over a Neural Network to check the accuracy
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIEvaluation implements Serializable {

	private static final long serialVersionUID = 1L;

	/**
	 * Human readable description of the network performance
	 */
	private String report;

	/**
	 * HTML representation of the confusion matrix
	 */
	private String confusionMatrix;

	private Date lastEvaluated;

	/**
	 * Indicates if there is a currently running evaluation process
	 */
	private boolean evaluating = false;

	public GUIEvaluation() {
		super();
	}

	public GUIEvaluation(String stats, String confusionMatrix) {
		super();
		this.report = stats;
		this.confusionMatrix = confusionMatrix;
	}

	public String getReport() {
		return report;
	}

	public void setReport(String report) {
		this.report = report;
	}

	public String getConfusionMatrix() {
		return confusionMatrix;
	}

	public void setConfusionMatrix(String confusionMatrix) {
		this.confusionMatrix = confusionMatrix;
	}

	public Date getLastEvaluated() {
		return lastEvaluated;
	}

	public void setLastEvaluated(Date lastEvaluated) {
		this.lastEvaluated = lastEvaluated;
	}

	public boolean isEvaluating() {
		return evaluating;
	}

	public void setEvaluating(boolean evaluating) {
		this.evaluating = evaluating;
	}
}
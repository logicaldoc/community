package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * The algorithm to use to optimize the update of the weights inside a neural
 * network.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class GUIUpdater implements Serializable {

	private static final long serialVersionUID = 1L;

	public static final String ADAGRAD = "ADAGRAD";

	public static final String NESTEROV = "NESTEROV";

	private String updateAlgorithm = ADAGRAD;

	private Double learningRate;

	private Double epsilon;

	private Double momentum;

	public String getUpdateAlgorithm() {
		return updateAlgorithm;
	}

	public void setUpdateAlgorithm(String updateAlgorithm) {
		this.updateAlgorithm = updateAlgorithm;
	}

	public Double getLearningRate() {
		return learningRate;
	}

	public void setLearningRate(Double learningRate) {
		this.learningRate = learningRate;
	}

	public Double getEpsilon() {
		return epsilon;
	}

	public void setEpsilon(Double epsilon) {
		this.epsilon = epsilon;
	}

	public Double getMomentum() {
		return momentum;
	}

	public void setMomentum(Double momentum) {
		this.momentum = momentum;
	}
}
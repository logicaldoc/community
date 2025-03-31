package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * GUI repesentation of a prediction result
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIPredictionResult implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	/**
	 * Probability (0..1) of this result
	 */
	private double score = 0L;

	private Object value;

	public GUIPredictionResult(String name, double score, Object value) {
		super();
		this.name = name;
		this.score = score;
		this.value = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getScore() {
		return score;
	}

	public void setScore(double score) {
		this.score = score;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
	}
}
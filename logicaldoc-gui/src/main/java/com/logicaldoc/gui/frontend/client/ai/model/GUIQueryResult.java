package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * GUI repesentation of a prediction result
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIQueryResult implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	/**
	 * Probability (0..1) of this result
	 */
	private double score = 0L;

	private String value;

	public GUIQueryResult() {
		super();
	}
	
	public GUIQueryResult(String name, double score, String value) {
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

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
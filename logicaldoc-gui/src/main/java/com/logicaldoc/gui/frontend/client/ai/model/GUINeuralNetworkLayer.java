package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * A GUI bean representing a layer of a neural network
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUINeuralNetworkLayer implements Serializable {

	private static final long serialVersionUID = 1L;

	private String name;

	/**
	 * Number of output nodes
	 */
	private int outputNodes = 3;

	private String activation = "RELU";

	public GUINeuralNetworkLayer() {
		super();
	}

	public GUINeuralNetworkLayer(String name, int outputNodes, String activation) {
		super();
		this.name = name;
		this.outputNodes = outputNodes;
		this.activation = activation;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getOutputNodes() {
		return outputNodes;
	}

	public void setOutputNodes(int outputNodes) {
		this.outputNodes = outputNodes;
	}

	public String getActivation() {
		return activation;
	}

	public void setActivation(String activation) {
		this.activation = activation;
	}

}
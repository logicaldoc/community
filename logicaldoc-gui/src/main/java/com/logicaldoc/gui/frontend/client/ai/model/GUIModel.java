package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * A GUI bean representing an AI model
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUIModel implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private String type = "neural";

	private String features;

	private String categories;

	private String activation = "RELU";

	private String weightInit = "XAVIER";

	private String loss = "NEGATIVELOGLIKELIHOOD";

	private List<GUINeuralNetworkLayer> layers = new ArrayList<>();

	private int batch = 200;

	private long seed = 123;

	private GUITraining training = new GUITraining();

	public GUIModel(long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public GUIModel() {
		super();
	}

	public long getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}

	public String getDescription() {
		return description;
	}

	public String getType() {
		return type;
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getFeatures() {
		return features;
	}

	public String getCategories() {
		return categories;
	}

	public void setFeatures(String features) {
		this.features = features;
	}

	public void setCategories(String categories) {
		this.categories = categories;
	}

	public String getActivation() {
		return activation;
	}

	public void setActivation(String activation) {
		this.activation = activation;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getWeightInit() {
		return weightInit;
	}

	public void setWeightInit(String weightInit) {
		this.weightInit = weightInit;
	}

	public String getLoss() {
		return loss;
	}

	public void setLoss(String loss) {
		this.loss = loss;
	}

	public List<GUINeuralNetworkLayer> getLayers() {
		return layers;
	}

	public void setLayers(List<GUINeuralNetworkLayer> layers) {
		this.layers = layers;
	}

	public int getBatch() {
		return batch;
	}

	public void setBatch(int batch) {
		this.batch = batch;
	}

	public long getSeed() {
		return seed;
	}

	public void setSeed(long seed) {
		this.seed = seed;
	}

	public GUITraining getTraining() {
		return training;
	}

	public void setTraining(GUITraining training) {
		this.training = training;
	}
}
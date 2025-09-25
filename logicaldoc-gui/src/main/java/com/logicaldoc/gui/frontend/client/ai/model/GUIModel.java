package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

	private int cutoff = 1;

	private int ngramMin = 2;

	private int ngramMax = 4;

	private String language = "en";

	private GUITraining training = new GUITraining();

	private GUIEvaluation evaluation = new GUIEvaluation();

	private GUIUpdater updater = new GUIUpdater();

	public GUIModel(long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public GUIModel() {
		super();
	}

	public int getCutoff() {
		return cutoff;
	}

	public void setCutoff(int cutoff) {
		this.cutoff = cutoff;
	}

	public int getNgramMin() {
		return ngramMin;
	}

	public void setNgramMin(int ngramMin) {
		this.ngramMin = ngramMin;
	}

	public int getNgramMax() {
		return ngramMax;
	}

	public void setNgramMax(int ngramMax) {
		this.ngramMax = ngramMax;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
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

	public List<String> getFeaturesList() {
		return Stream.of(features.split(",")).map(String::trim).collect(Collectors.toList());
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

	public GUIEvaluation getEvaluation() {
		return evaluation;
	}

	public void setEvaluation(GUIEvaluation evaluation) {
		this.evaluation = evaluation;
	}

	public boolean isNeuralNetwork() {
		return "neural".equals(type);
	}

	public GUIUpdater getUpdater() {
		return updater;
	}

	public void setUpdater(GUIUpdater updater) {
		this.updater = updater;
	}
}
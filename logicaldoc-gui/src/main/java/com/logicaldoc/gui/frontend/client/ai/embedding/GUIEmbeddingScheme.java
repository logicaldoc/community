package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.io.Serializable;

/**
 * A GUI bean representing an embedding scheme
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class GUIEmbeddingScheme implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name;

	private String label;

	private String model;

	private Long modelId;

	private String modelSpec;

	private String apiKey;

	private int chunksBatch = 50;

	private boolean enabled = true;

	private String type = "mariadb";

	private Integer vectorSize;

	private Integer batch;

	public GUIEmbeddingScheme() {
	}

	public GUIEmbeddingScheme(long id, String name) {
		this.id = id;
		this.name = name;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public Long getModelId() {
		return modelId;
	}

	public void setModelId(Long modelId) {
		this.modelId = modelId;
	}

	public String getModelSpec() {
		return modelSpec;
	}

	public void setModelSpec(String modelSpec) {
		this.modelSpec = modelSpec;
	}

	public String getApiKey() {
		return apiKey;
	}

	public void setApiKey(String apiKey) {
		this.apiKey = apiKey;
	}

	public int getChunksBatch() {
		return chunksBatch;
	}

	public void setChunksBatch(int chunksBatch) {
		this.chunksBatch = chunksBatch;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Integer getVectorSize() {
		return vectorSize;
	}

	public void setVectorSize(Integer vectorSize) {
		this.vectorSize = vectorSize;
	}

	public Integer getBatch() {
		return batch;
	}

	public void setBatch(Integer batch) {
		this.batch = batch;
	}
}
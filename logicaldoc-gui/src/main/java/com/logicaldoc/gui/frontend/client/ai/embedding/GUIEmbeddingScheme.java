package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.io.Serializable;
import java.util.Date;

public class GUIEmbeddingScheme implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private String name;

	private String label;

	private String description;

	private Long embedderId;

	private String embedderName;

	private int dimensions;

	private String language;

	private String tokenizer;

	private boolean enabled = true;

	private boolean defaultScheme = false;

	private long embeddingsCount;

	private long documentsIndexed;

	private long tenantId;

	private Date created;

	private Date lastModified;

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

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Long getEmbedderId() {
		return embedderId;
	}

	public void setEmbedderId(Long embedderId) {
		this.embedderId = embedderId;
	}

	public String getEmbedderName() {
		return embedderName;
	}

	public void setEmbedderName(String embedderName) {
		this.embedderName = embedderName;
	}

	public int getDimensions() {
		return dimensions;
	}

	public void setDimensions(int dimensions) {
		this.dimensions = dimensions;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getTokenizer() {
		return tokenizer;
	}

	public void setTokenizer(String tokenizer) {
		this.tokenizer = tokenizer;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public boolean isDefaultScheme() {
		return defaultScheme;
	}

	public void setDefaultScheme(boolean defaultScheme) {
		this.defaultScheme = defaultScheme;
	}

	public long getEmbeddingsCount() {
		return embeddingsCount;
	}

	public void setEmbeddingsCount(long embeddingsCount) {
		this.embeddingsCount = embeddingsCount;
	}

	public long getDocumentsIndexed() {
		return documentsIndexed;
	}

	public void setDocumentsIndexed(long documentsIndexed) {
		this.documentsIndexed = documentsIndexed;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public Date getCreated() {
		return created;
	}

	public void setCreated(Date created) {
		this.created = created;
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(Date lastModified) {
		this.lastModified = lastModified;
	}

}

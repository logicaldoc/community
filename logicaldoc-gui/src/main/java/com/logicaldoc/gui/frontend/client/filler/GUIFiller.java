package com.logicaldoc.gui.frontend.client.filler;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * A GUI bean representing an AI filler
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class GUIFiller implements Serializable {

    private static final long serialVersionUID = 1L;

    private long id = 0;

    private String name;

    private String label;

    private String description;

    private String type;

    private String model;

    private Long modelId;

    private Long embeddingSchemeId;

    private Double threshold = 0.7d;

    private String attribute;

    private String candidate;

    private String format;

    private String decimalSeparator = ",";

    private String groupingSeparator = ".";
    
    private String exclusionRegex;

	private String inclusionRegex;

    private List<GUIFiller> chain = new ArrayList<>();

    private boolean overwrite;

    private boolean onCheckin;

    private String automation;
    
    private String automationBefore;

	private List<GUICriterion> criteria = new ArrayList<>();

    public GUIFiller() {
        // Empty constructor
    }

    public GUIFiller(long id, String name) {
        super();
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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getModelId() {
        return modelId;
    }

    public void setModelId(Long modelId) {
        this.modelId = modelId;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public Double getThreshold() {
        return threshold;
    }

    public void setThreshold(Double threshold) {
        this.threshold = threshold;
    }

    public List<GUIFiller> getChain() {
        return chain;
    }

    public void setChain(List<GUIFiller> chain) {
        this.chain = chain;
    }

    public Long getEmbeddingSchemeId() {
        return embeddingSchemeId;
    }

    public void setEmbeddingSchemeId(Long embeddingSchemeId) {
        this.embeddingSchemeId = embeddingSchemeId;
    }

    public boolean isOverwrite() {
        return overwrite;
    }

    public void setOverwrite(boolean overwrite) {
        this.overwrite = overwrite;
    }

    public boolean isOnCheckin() {
        return onCheckin;
    }

    public void setOnCheckin(boolean onCheckin) {
        this.onCheckin = onCheckin;
    }

    public String getAutomation() {
        return automation;
    }

    public void setAutomation(String automation) {
        this.automation = automation;
    }
    
    public String getAutomationBefore() {
		return automationBefore;
	}

	public void setAutomationBefore(String automationBefore) {
		this.automationBefore = automationBefore;
	}

    public String getAttribute() {
        return attribute;
    }

    public void setAttribute(String attribute) {
        this.attribute = attribute;
    }

    public String getCandidate() {
        return candidate;
    }

    public void setCandidate(String candidate) {
        this.candidate = candidate;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String getDecimalSeparator() {
        return decimalSeparator;
    }

    public void setDecimalSeparator(String decimalSeparator) {
        this.decimalSeparator = decimalSeparator;
    }

    public String getGroupingSeparator() {
        return groupingSeparator;
    }

    public void setGroupingSeparator(String groupingSeparator) {
        this.groupingSeparator = groupingSeparator;
    }

    public List<GUICriterion> getCriteria() {
        return criteria;
    }

    public void setCriteria(List<GUICriterion> criteria) {
        this.criteria = criteria;
    }
    
    public String getExclusionRegex() {
		return exclusionRegex;
	}

	public void setExclusionRegex(String exclusionRegex) {
		this.exclusionRegex = exclusionRegex;
	}

	public String getInclusionRegex() {
		return inclusionRegex;
	}

	public void setInclusionRegex(String inclusionRegex) {
		this.inclusionRegex = inclusionRegex;
	}
}
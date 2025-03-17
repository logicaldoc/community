package com.logicaldoc.gui.frontend.client.ai.sampler;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIExtensibleObject;
import com.logicaldoc.gui.common.client.beans.GUIFolder;

/**
 * A GUI bean representing a sampler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 */
public class GUISampler extends GUIExtensibleObject {

	private static final long serialVersionUID = 1L;

	private String name;

	private String label;

	private String description;

	private String type = "csv";

	private long count = 0L;

	private GUIDocument document;

	private GUIFolder folder;

	private List<GUISampler> samplers = new ArrayList<>();

	private List<GUIExtensibleObject> source = new ArrayList<>();

	private String categoryAttribute = null;

	private String automation;

	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}

	public String getDescription() {
		return description;
	}

	public long getCount() {
		return count;
	}

	public GUIDocument getDocument() {
		return document;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public List<GUISampler> getSamplers() {
		return samplers;
	}

	public List<GUIExtensibleObject> getSource() {
		return source;
	}

	public String getCategoryAttribute() {
		return categoryAttribute;
	}

	public String getAutomation() {
		return automation;
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

	public void setCount(long count) {
		this.count = count;
	}

	public void setDocument(GUIDocument document) {
		this.document = document;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
	}

	public void setSamplers(List<GUISampler> samplers) {
		this.samplers = samplers;
	}

	public void setSource(List<GUIExtensibleObject> source) {
		this.source = source;
	}

	public void setCategoryAttribute(String categoryAttribute) {
		this.categoryAttribute = categoryAttribute;
	}

	public void setAutomation(String automation) {
		this.automation = automation;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
}
package com.logicaldoc.gui.frontend.client.ai.sampler;

import java.io.Serializable;
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
 */
public class GUISampler implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String name;

	private String label;

	private String description;

	private String type = "csv";

	private long count = 0L;

	private GUIDocument document;

	private GUIFolder folder;

	private List<GUISampler> chain = new ArrayList<>();

	private List<GUIExtensibleObject> source = new ArrayList<>();

	private String categoryAttribute = null;

	private String automation;

	private String delimiter = ",";

	private String quote = "\"";

	/**
	 * The index of the column(starting form 0) containing the category.<br>
	 * value -1 means the last column<br>
	 * value -2 means no category column at all
	 */
	private int categoryIndex = -1;
	
	public GUISampler(long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public GUISampler() {
		super();
	}
	
	public String getDelimiter() {
		return delimiter;
	}

	public String getQuote() {
		return quote;
	}

	public int getCategoryIndex() {
		return categoryIndex;
	}

	public void setDelimiter(String delimiter) {
		this.delimiter = delimiter;
	}

	public void setQuote(String quote) {
		this.quote = quote;
	}

	public void setCategoryIndex(int categoryIndex) {
		this.categoryIndex = categoryIndex;
	}

	public String getName() {
		return name;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
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

	public List<GUISampler> getChain() {
		return chain;
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

	public void setChain(List<GUISampler> chain) {
		this.chain = chain;
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
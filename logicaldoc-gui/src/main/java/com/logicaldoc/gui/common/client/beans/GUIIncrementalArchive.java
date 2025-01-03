package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * An incremental export archive configuration
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIIncrementalArchive implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id = 0;

	private String prefix;

	private int frequency = 1;

	private GUIFolder folder;

	private int type = GUIArchive.TYPE_DEFAULT;

	private List<GUITemplate> templates = new ArrayList<>();

	private Long aosManagerId;

	public GUIIncrementalArchive() {
		folder = new GUIFolder();
		folder.setId(5);
		folder.setName("/");
	}

	public String getPrefix() {
		return prefix;
	}

	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public int getFrequency() {
		return frequency;
	}

	public void setFrequency(int frequency) {
		this.frequency = frequency;
	}

	public GUIFolder getFolder() {
		return folder;
	}

	public void setFolder(GUIFolder folder) {
		this.folder = folder;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public List<GUITemplate> getTemplates() {
		return templates;
	}

	public void setTemplates(List<GUITemplate> templates) {
		this.templates = templates;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public List<Long> getTemplateIds() {
		return templates.stream().map(t -> t.getId()).collect(Collectors.toUnmodifiableList());
	}

	public Long getAosManagerId() {
		return aosManagerId;
	}

	public void setAosManagerId(Long aosManagerId) {
		this.aosManagerId = aosManagerId;
	}
}
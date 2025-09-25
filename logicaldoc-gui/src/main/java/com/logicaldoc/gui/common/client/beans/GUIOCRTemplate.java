package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;

/**
 * This models an OCR template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class GUIOCRTemplate implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	/**
	 * Unique name of the template
	 */
	private String name;

	private String description;

	/**
	 * Sample image displayed to the user in order to define the zones.
	 */
	private String sample;

	/**
	 * The assigned document template
	 */
	private GUITemplate template;

	/**
	 * The zones
	 */
	protected ArrayList<GUIZone> zones = new ArrayList<>();

	private int batch = 200;

	private boolean saveChangeEvent = true;

	public GUIOCRTemplate() {
		try {
			batch = Session.get().getConfigAsInt("zonalocr.batch");
		} catch (Exception t) {
			// Nothing to do
		}
	}

	public GUIZone getZone(String name) {
		for (GUIZone att : getZones()) {
			if (att.getName().equals(name))
				return att;
		}
		return null;
	}

	public void appendZone(GUIZone zone) {
		zones.add(zone);
	}

	public void removeZone(String name) {
		if (getZone(name) == null)
			return;

		ArrayList<GUIZone> newAttrs = new ArrayList<>();
		for (GUIZone att : zones)
			if (!att.getName().equals(name))
				newAttrs.add(att);

		zones = newAttrs;
	}

	public List<GUIZone> getZones() {
		return zones;
	}

	public void setZones(List<GUIZone> zones) {
		this.zones.clear();
		this.zones.addAll(zones);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getSample() {
		return sample;
	}

	public void setSample(String sample) {
		this.sample = sample;
	}

	public GUITemplate getTemplate() {
		return template;
	}

	public void setTemplate(GUITemplate template) {
		this.template = template;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public int getBatch() {
		return batch;
	}

	public void setBatch(int batch) {
		this.batch = batch;
	}

	public boolean isSaveChangeEvent() {
		return saveChangeEvent;
	}

	public void setSaveChangeEvent(boolean saveChangeEvent) {
		this.saveChangeEvent = saveChangeEvent;
	}
}
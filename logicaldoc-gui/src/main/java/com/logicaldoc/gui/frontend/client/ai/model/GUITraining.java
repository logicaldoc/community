package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;
import java.util.Date;

import com.logicaldoc.gui.frontend.client.ai.sampler.GUISampler;

/**
 * A GUI bean representing a model's training infomation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class GUITraining implements Serializable {

	private static final long serialVersionUID = 1L;

	private Date lastTrained;

	private String report;

	private int epochs = 1000;

	private String cron;

	private boolean enabled = true;
	
	/**
	 * Indicates if there is a currently running training process
	 */
	private boolean training = false;
	
	/**
	 * Indicates if input training data must be persisted
	 */
	private boolean saveSamples = true;

	private boolean trainable = true;
	
	private GUISampler sampler;
	
	public boolean isTrainable() {
		return trainable;
	}

	public void setTrainable(boolean trainable) {
		this.trainable = trainable;
	}

	public Date getLastTrained() {
		return lastTrained;
	}

	public void setLastTrained(Date lastTrained) {
		this.lastTrained = lastTrained;
	}

	public int getEpochs() {
		return epochs;
	}

	public void setEpochs(int epochs) {
		this.epochs = epochs;
	}

	public String getCron() {
		return cron;
	}

	public void setCron(String cron) {
		this.cron = cron;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public GUISampler getSampler() {
		return sampler;
	}

	public void setSampler(GUISampler sampler) {
		this.sampler = sampler;
	}

	public String getReport() {
		return report;
	}

	public void setReport(String report) {
		this.report = report;
	}
	
	public boolean isTraining() {
		return training;
	}

	public void setTraining(boolean training) {
		this.training = training;
	}

	public boolean isSaveSamples() {
		return saveSamples;
	}

	public void setSaveSamples(boolean saveSamples) {
		this.saveSamples = saveSamples;
	}
}
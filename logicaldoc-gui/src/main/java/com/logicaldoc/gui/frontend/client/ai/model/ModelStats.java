package com.logicaldoc.gui.frontend.client.ai.model;

/**
 * Shows model's statistics.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelStats extends ModelDetailsTab {

	public ModelStats(GUIModel model) {
		super(model, null);
		setWidth100();
		setHeight100();
		addMember(new AIStatsPanel(model.getId()));
	}

	boolean validate() {
		return true;
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
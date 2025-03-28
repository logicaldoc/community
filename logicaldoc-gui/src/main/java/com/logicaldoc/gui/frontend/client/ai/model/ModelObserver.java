package com.logicaldoc.gui.frontend.client.ai.model;

/**
 * A listener to react to model changes
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 *
 */
public interface ModelObserver {

	/**
	 * Called when something changes in the model
	 * 
	 * @param model The updated model
	 */
	public void onModelChanged(GUIModel model);
}

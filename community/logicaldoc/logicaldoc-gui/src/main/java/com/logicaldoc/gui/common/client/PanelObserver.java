package com.logicaldoc.gui.common.client;

/**
 * A listener on relevant events on the main panel.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.7
 */
public interface PanelObserver {
	/**
	 * Invoked after the panel ahs been selected
	 * 
	 * @param panel the name of the selected panel
	 */
	public void onTabSeleted(String panel);
}

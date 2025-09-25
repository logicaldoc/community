package com.logicaldoc.gui.frontend.client.ai.model;

import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Superclass for all tab panels in the model details area
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public abstract class ModelDetailsTab extends HLayout {
	protected GUIModel model;

	protected ChangedHandler changedHandler;

	/**
	 * Constructor
	 * 
	 * @param model The model this instance refers to
	 * @param changedHandler The handler to be invoked in case of changes in the
	 *        model
	 */
	protected ModelDetailsTab(GUIModel model, ChangedHandler changedHandler) {
		super();
		this.model = model;
		this.changedHandler = changedHandler;
	}

	public GUIModel getModel() {
		return model;
	}

	public ChangedHandler getChangedHandler() {
		return changedHandler;
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
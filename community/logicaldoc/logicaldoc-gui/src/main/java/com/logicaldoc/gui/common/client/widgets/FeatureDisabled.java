package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Simple panel to show the user that a feature is disabled.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FeatureDisabled extends VLayout {
	public FeatureDisabled() {
		this(null);
	}

	public FeatureDisabled(Integer feature) {
		Label label = null;
		if (feature == null)
			label = new MessageLabel(new GUIMessage("<b>" + I18N.message("featuredisabled") + "</b>",
					GUIMessage.PRIO_WARN), false);
		else
			label = new MessageLabel(new GUIMessage("<b>" + I18N.message("Feature_" + feature) + " "
					+ I18N.message("disabled") + "</b>", GUIMessage.PRIO_WARN), false);
		label.setHeight(30);
		label.setPadding(10);
		label.setAlign(Alignment.CENTER);
		label.setValign(VerticalAlignment.CENTER);
		label.setShowEdges(false);

		addMember(label);
	}
}
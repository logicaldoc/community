package com.logicaldoc.gui.frontend.client.dashboard;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.search.TagsForm;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Portlet;

/**
 * Portlet specialized in allowing the user to search by tag.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagsPortlet extends Portlet {

	private HLayout container = null;

	public TagsPortlet() {
		if (Feature.enabled(Feature.TAGS)) {
			refresh();
		} else
			addItem(new FeatureDisabled());
	}

	private void refresh() {
		setTitle(AwesomeFactory.getIconHtml("tag", I18N.message("tags")));

		if (container != null)
			removeChild(container);

		setHeaderControls(HeaderControls.HEADER_LABEL);

		setCanDrag(false);
		setCanDrop(false);
		setDragAppearance(DragAppearance.OUTLINE);
		setDragOpacity(30);

		container = new HLayout();
		container.setWidth100();
		container.setHeight100();
		container.setAlign(Alignment.CENTER);
		container.setMargin(25);

		addChild(container);

		container.addMember(new TagsForm(false, false));
	}

}
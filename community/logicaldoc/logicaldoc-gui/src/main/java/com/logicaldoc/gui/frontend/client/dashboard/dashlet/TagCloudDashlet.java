package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.dashboard.TagCloud;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.DragAppearance;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Dashlet specialized in showing the tag cloud in a 3-D fashon.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.6
 */
public class TagCloudDashlet extends Dashlet {
	private HLayout container = null;

	public TagCloudDashlet(int id) {
		super(id);
		if (Feature.enabled(Feature.TAGS)) {
			setMinHeight(200);

			HeaderControl refresh = new HeaderControl(HeaderControl.REFRESH, new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					refresh();
				}
			});

			setTitle(AwesomeFactory.getIconHtml("tag", I18N.message("tagcloud")));

			if (getId() == Constants.DASHLET_TAGCLOUD) {
				setHeaderControls(HeaderControls.HEADER_LABEL, refresh, HeaderControls.MAXIMIZE_BUTTON,
						HeaderControls.CLOSE_BUTTON);
				setCanDrag(true);
				setCanDrop(true);
			} else
				setHeaderControls(HeaderControls.HEADER_LABEL, refresh);

			setDragAppearance(DragAppearance.OUTLINE);
			setDragOpacity(30);

			refresh();

			addResizedHandler(new ResizedHandler() {

				@Override
				public void onResized(ResizedEvent event) {
					refresh();
				}
			});

		} else
			addItem(new FeatureDisabled());
	}

	public TagCloudDashlet() {
		this(Constants.DASHLET_TAGCLOUD);
	}

	public void refresh() {
		if (container != null)
			removeChild(container);

		container = new HLayout();
		container.setWidth100();
		container.setHeight100();
		container.setAlign(Alignment.CENTER);
		container.setMargin(25);

		addChild(container);

		TagCloud tc = new TagCloud();
		tc.setWidth(getWidth() - 20);
		tc.setHeight(getHeight() - 20);
		container.addMember(tc);
		tc.refresh();
	}
}
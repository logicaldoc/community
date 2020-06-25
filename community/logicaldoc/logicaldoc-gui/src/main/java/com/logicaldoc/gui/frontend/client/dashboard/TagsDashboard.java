package com.logicaldoc.gui.frontend.client.dashboard;

import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.frontend.client.dashboard.dashlet.TagCloudDashlet;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import com.smartgwt.client.widgets.layout.PortalLayout;

/**
 * User dashboard that displays several portlets like a portal page.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagsDashboard extends PortalLayout {

	private TagCloudDashlet cloud = null;

	private MostUsedTagsPortlet mostUsed = null;

	private TagsPortlet tags = null;

	public TagsDashboard() {
		setShowColumnMenus(false);
		setShowEdges(false);
		setShowShadow(false);
		setCanDrag(false);
		setCanDrop(false);
		setColumnBorder("0px");

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				if (cloud != null)
					cloud.refresh();
			}
		});
	}

	@Override
	public void onDraw() {
		if (cloud != null)
			removePortlet(cloud);

		if (mostUsed != null)
			removePortlet(mostUsed);

		if (tags != null)
			removePortlet(tags);

		// Place the portlets
		mostUsed = new MostUsedTagsPortlet();
		addPortlet(mostUsed, 0, 0);

		tags = new TagsPortlet();
		addPortlet(tags, 0, 1);

		GUIDashlet tagsDashlet=new GUIDashlet();
		tagsDashlet.setName("tagcloud");
		tagsDashlet.setTitle("tagcloud");
		tagsDashlet.setType("content");
		tagsDashlet.setId(0L);
		cloud = new TagCloudDashlet(tagsDashlet);
		addPortlet(cloud, 1, 0);

		cloud.refresh();
	}
}
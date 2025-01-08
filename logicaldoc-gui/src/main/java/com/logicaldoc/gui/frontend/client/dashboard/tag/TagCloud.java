package com.logicaldoc.gui.frontend.client.dashboard.tag;

import java.util.List;

import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUITag;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;

import gdurelle.tagcloud.client.tags.WordTag;

/**
 * This represent a tag cloud using a 3-D ball.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TagCloud extends VLayout {

	private Layout layout = null;

	public TagCloud() {
		addVisibilityChangedHandler(event -> refresh());
	}

	public void refresh() {
		TagService.Instance.get().getTagCloud(new IgnoreAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUITag> tags) {
				refresh(tags);
			}
		});
	}

	private void refresh(List<GUITag> tags) {
		if (layout != null) {
			removeMember(layout);
			layout = null;
		}

		gdurelle.tagcloud.client.tags.TagCloud tagCloud = new gdurelle.tagcloud.client.tags.TagCloud();

		for (GUITag tag : tags) {
			WordTag word = new WordTag(tag.getTag(), "javascript:searchTag(\"" + tag.getTag() + "\");");
			word.setNumberOfOccurences(Integer.parseInt(Long.toString(tag.getCount())));
			tagCloud.addWord(word);
		}

		tagCloud.setPixelSize(getWidth() - 20, getHeight() - 2);

		layout = new HLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.addMember(tagCloud.asWidget());

		addMember(layout);
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
package com.logicaldoc.gui.common.client.preview;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Displays a clickable title for previewing the document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3.1
 */
public class PreviewTile extends HLayout {

	private long docId;

	private String title;

	public PreviewTile(final long docId, final String title) {
		this.docId = docId;
		this.title = title;
		setMembersMargin(1);
		setAlign(Alignment.RIGHT);
		setOverflow(Overflow.HIDDEN);

		initGUI();
	}

	private void initGUI() {
		Canvas[] members = getMembers();
		if (members != null && members.length > 0)
			for (Canvas canvas : members)
				removeChild(canvas);

		if (Session.get().isShowThumbnail()) {
			String html = "<img border='0' alt='' title='' src='" + Util.thumbnailUrl(docId, null) + "' height='"
					+ Session.get().getConfig("gui.thumbnail.size") + "px' style='float:body;' align='body'/>";
			HTMLFlow thumbnailImage = new HTMLFlow(html);
			thumbnailImage.addClickHandler(event -> {
				if (Session.get().isShowThumbnail()) {
					new TileImageLightbox(docId, title).show();
				}
			});

			Img closeThumbnailImage = new Img("[SKIN]/icons/rectangle-xmark.png", 16, 16);
			closeThumbnailImage.setShowRollOver(true);
			closeThumbnailImage.addClickHandler(event -> {
				Session.get().setShowThumbnail(false);
				initGUI();
				event.cancel();
			});

			setMembers(thumbnailImage, closeThumbnailImage);
		} else {
			IButton showThumbnail = new IButton(I18N.message("showthumbnail"));
			showThumbnail.addClickHandler(event -> {
				Session.get().setShowThumbnail(true);
				initGUI();
				event.cancel();
			});
			setMembers(showThumbnail);
		}
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
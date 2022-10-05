package com.logicaldoc.gui.common.client.widgets.preview;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageLightbox;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
			HTMLFlow tileImage = new HTMLFlow(html);
			tileImage.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					if (Session.get().isShowThumbnail()) {
						ImageLightbox lightbox = new ImageLightbox(docId, title);
						lightbox.show();
					}
				}
			});

			Img closeTileImage = new Img("[SKIN]/headerIcons/close.gif", 16, 16);
			closeTileImage.setShowRollOver(true);
			closeTileImage.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					Session.get().setShowThumbnail(false);
					initGUI();
					event.cancel();
				}
			});

			setMembers(tileImage, closeTileImage);
		} else {
			IButton showThumbnail = new IButton(I18N.message("showthumbnail"));
			showThumbnail.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					Session.get().setShowThumbnail(true);
					initGUI();
					event.cancel();
				}
			});
			setMembers(showThumbnail);
		}
	}
}

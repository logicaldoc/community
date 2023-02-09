package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays the title for the given folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class FolderTile extends HLayout {

	private GUIFolder folder;

	private ChangedHandler changedHandler;

	public FolderTile(GUIFolder folder, ChangedHandler changedHandler) {
		this.folder = folder;
		this.changedHandler = changedHandler;

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
			String html = "<img border='0' alt='' title='' src='" + folder.getTile() + "' height='"
					+ Session.get().getConfig("gui.thumbnail.size") + "px' style='float:body;' align='body'/>";
			HTMLFlow tileImage = new HTMLFlow(html);

			Img close = new Img("[SKIN]/cancel.png", 16, 16);
			close.setShowRollOver(true);
			close.addClickHandler((ClickEvent evn) -> {
				Session.get().setShowThumbnail(false);
				initGUI();
			});

			Img changeImage = new Img("[SKIN]/cog.png", 16, 16);
			changeImage.setShowRollOver(true);
			changeImage.setTooltip(I18N.message("changeimage"));
			changeImage.addClickHandler((ClickEvent evn) -> new FolderImageUploader(folder, (ChangedEvent ev) -> {
				initGUI();
				changedHandler.onChanged(null);
			}).show());

			VLayout icons = new VLayout();
			icons.setMembersMargin(2);
			icons.setWidth(16);
			if (folder.getTile() != null && !folder.getTile().isEmpty())
				icons.addMember(close);
			if (changedHandler != null)
				icons.addMember(changeImage);

			setMembers(tileImage, icons);
		} else {
			IButton showThumbnail = new IButton(I18N.message("showtile"));
			showThumbnail.addClickHandler((ClickEvent event) -> {
				Session.get().setShowThumbnail(true);
				initGUI();
			});
			setMembers(showThumbnail);
		}
	}
}

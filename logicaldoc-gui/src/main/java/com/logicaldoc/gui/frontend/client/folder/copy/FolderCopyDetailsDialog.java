package com.logicaldoc.gui.frontend.client.folder.copy;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.StickyWindow;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to allow the users to input the data when copying a
 * folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class FolderCopyDetailsDialog extends StickyWindow {

	private GUIFolder metadata;

	private String securityPolicy;

	private boolean foldersOnly;

	private long targetFolderId;

	public FolderCopyDetailsDialog(GUIFolder metadata, long targetFolderId, String securityPolicy,
			boolean foldersOnly) {
		super("copyfolder");
		setIsModal(true);
		this.metadata = metadata;
		this.securityPolicy = securityPolicy;
		this.foldersOnly = foldersOnly;
		this.targetFolderId = targetFolderId;
	}

	@Override
	protected WindowStatus getDefaultStatus() {
		return new WindowStatus(800, 350);
	}

	@Override
	protected void onDraw() {
		super.onDraw();

		FolderCopyDetailsPanel detailsPanel = new FolderCopyDetailsPanel(metadata);
		detailsPanel.setWidth100();
		detailsPanel.setHeight("*");
		detailsPanel.setShowResizeBar(false);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("60%");
		spacer.setOverflow(Overflow.HIDDEN);

		Button copy = new Button(I18N.message("copy"));
		copy.setAutoFit(true);
		copy.addClickHandler(event -> {
			if (!detailsPanel.validate())
				return;

			LD.contactingServer();
			FolderService.Instance.get().copyFolders(Arrays.asList(metadata.getId()), targetFolderId, foldersOnly,
					securityPolicy, metadata, new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void arg) {
							LD.clearPrompt();
							destroy();
						}
					});
		});

		HLayout savePanel = new HLayout();
		savePanel.addMember(copy);
		savePanel.addMember(spacer);
		savePanel.setWidth100();
		savePanel.setHeight(30);
		savePanel.setMargin(2);
		savePanel.setMembersMargin(10);

		VLayout content = new VLayout();
		content.setTop(10);
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(3);
		content.setMembers(detailsPanel, savePanel);

		addItem(content);
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
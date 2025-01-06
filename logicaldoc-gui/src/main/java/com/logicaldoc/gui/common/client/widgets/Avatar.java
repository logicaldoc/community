package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.GUIAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * Displays an Avatar
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class Avatar extends HLayout {

	private static final String GUI_AVATAR_SIZE = "gui.avatar.size";

	private long userId;

	private AsyncCallback<Void> callback;

	public Avatar(long userId) {
		this(userId, null);
	}

	public Avatar(long userId, AsyncCallback<Void> callback) {
		this.userId = userId;
		this.callback = callback;

		setMembersMargin(1);
		setAlign(Alignment.LEFT);
		setOverflow(Overflow.HIDDEN);

		initGUI();
	}

	private void initGUI() {
		Canvas[] members = getMembers();
		if (members != null && members.length > 0)
			for (Canvas canvas : members)
				removeChild(canvas);

		int avatarSize = Session.get().getConfigAsInt(GUI_AVATAR_SIZE);
		Img avatarImage = new Img(Util.avatarUrl(Long.toString(userId), true), avatarSize, avatarSize);
		avatarImage.setLeft(0);
		avatarImage.setLayoutAlign(Alignment.LEFT);
		avatarImage.setAlign(Alignment.LEFT);
		avatarImage.setContextMenu(prepareContextMenu());
		setMembers(avatarImage);
	}

	private Menu prepareContextMenu() {
		MenuItem reset = new MenuItem();
		reset.setTitle(I18N.message("reset"));
		reset.addClickHandler(event -> SecurityService.Instance.get().resetAvatar(userId, new GUIAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				Avatar.this.initGUI();
				if (callback != null)
					callback.onSuccess(null);
			}
		}));

		MenuItem update = new MenuItem();
		update.setTitle(I18N.message("update"));
		update.addClickHandler(event -> {
			Uploader uploader = new Uploader(userId);
			uploader.show();
		});

		Menu contextMenu = new Menu();
		contextMenu.setItems(reset, update);
		return contextMenu;
	}

	private class Uploader extends Window {

		private IButton saveButton;

		private Upload upload;

		private long userId;

		public Uploader(long userId) {
			this.userId = userId;

			setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
			setTitle(I18N.message("uploadavatar"));
			setCanDragResize(true);
			setIsModal(true);
			setShowModalMask(true);
			centerInPage();
			setMinWidth(300);
			setAutoSize(true);

			saveButton = new IButton(I18N.message("save"));
			saveButton.addClickHandler(event -> onSave());

			Label hint = new Label(I18N.message("avatarhint", Session.get().getConfig(GUI_AVATAR_SIZE),
					Session.get().getConfig(GUI_AVATAR_SIZE)));
			hint.setHeight(20);
			hint.setWrap(false);

			VLayout layout = new VLayout();
			layout.setMembersMargin(5);
			layout.setMargin(2);

			layout.addMember(hint);

			upload = new Upload(saveButton);
			upload.setFileTypes("*.png,*.jpg,*.jpeg,*.gif");
			layout.addMember(upload);
			layout.addMember(saveButton);

			addCloseClickHandler(event -> DocumentService.Instance.get().cleanUploadedFileFolder(new GUIAsyncCallback<>() {

				@Override
				public void onSuccess(Void result) {
					destroy();
				}
			}));

			addItem(layout);
		}

		public void onSave() {
			if (upload.getUploadedFile() == null) {
				SC.warn(I18N.message("filerequired"));
				return;
			}

			SecurityService.Instance.get().saveAvatar(userId, new GUIAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					super.onFailure(caught);
					close();
				}

				@Override
				public void onSuccess(Void arg) {
					Avatar.this.initGUI();
					DocumentService.Instance.get().cleanUploadedFileFolder(new GUIAsyncCallback<>() {

						@Override
						public void onSuccess(Void result) {
							destroy();
							close();
						}
					});
					if (callback != null)
						callback.onSuccess(arg);
				}
			});
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + (int) (userId ^ (userId >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Avatar other = (Avatar) obj;
		return userId == other.userId;
	}
}
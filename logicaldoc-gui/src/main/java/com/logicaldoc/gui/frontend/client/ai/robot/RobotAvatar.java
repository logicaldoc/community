package com.logicaldoc.gui.frontend.client.ai.robot;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
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
 * Displays a robot's avatar
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotAvatar extends HLayout {

	private static final String GUI_AVATAR_SIZE = "gui.avatar.size";

	private long robotId;

	private AsyncCallback<String> callback;

	public RobotAvatar(long robotId) {
		this(robotId, null);
	}

	public RobotAvatar(long robotId, AsyncCallback<String> callback) {
		this.robotId = robotId;
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

		RobotService.Instance.get().get(robotId, new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(GUIRobot rbt) {
				int avatarSize = Session.get().getConfigAsInt(GUI_AVATAR_SIZE);
				Img avatarImage = new Img(rbt.getAvatar(), avatarSize, avatarSize);
				avatarImage.setLeft(0);
				avatarImage.setLayoutAlign(Alignment.LEFT);
				avatarImage.setAlign(Alignment.LEFT);
				avatarImage.setContextMenu(prepareContextMenu());
				setMembers(avatarImage);

				if (callback != null)
					callback.onSuccess(rbt.getAvatar());
			}
		});
	}

	private Menu prepareContextMenu() {
		MenuItem reset = new MenuItem();
		reset.setTitle(I18N.message("reset"));
		reset.addClickHandler(event -> RobotService.Instance.get().resetAvatar(robotId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				initGUI();
			}
		}));

		MenuItem update = new MenuItem();
		update.setTitle(I18N.message("update"));
		update.addClickHandler(event -> new Uploader(robotId).show());

		Menu contextMenu = new Menu();
		contextMenu.setItems(reset, update);
		return contextMenu;
	}

	private class Uploader extends Window {

		private IButton saveButton;

		private Upload upload;

		private long robotId;

		public Uploader(long robotId) {
			this.robotId = robotId;

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
			upload.setFileTypes("*.png,*.jpg,*.jpeg,*.gif,*.svg");
			layout.addMember(upload);
			layout.addMember(saveButton);

			addCloseClickHandler(
					event -> DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {

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

			RobotService.Instance.get().saveAvatar(robotId, new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					super.onFailure(caught);
					close();
				}

				@Override
				public void onSuccess(Void arg) {
					initGUI();
					DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {

						@Override
						public void onSuccess(Void result) {
							destroy();
							close();
						}
					});
				}
			});
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

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
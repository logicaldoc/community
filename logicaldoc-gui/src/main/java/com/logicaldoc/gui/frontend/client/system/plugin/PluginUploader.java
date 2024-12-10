package com.logicaldoc.gui.frontend.client.system.plugin;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new plugin to install.
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class PluginUploader extends Window {
	private IButton submitButton;

	private Upload uploader;

	private PluginsPanel pluginsPanel;

	public PluginUploader(PluginsPanel pluginsPanel) {
		this.pluginsPanel = pluginsPanel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("uploadplugin"));
		setWidth(430);
		setHeight(170);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		submitButton = new IButton(I18N.message("install"));
		submitButton.addClickHandler(event -> onSubmit());

		VLayout layout = new VLayout();
		layout.setMembersMargin(1);
		layout.setMargin(2);

		uploader = new Upload(submitButton);
		uploader.setFileTypes("*.zip");
		layout.addMember(uploader);
		layout.addMember(submitButton);

		addItem(layout);

		addCloseClickHandler(event -> cleanUploads());
	}

	private void cleanUploads() {
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		SystemService.Instance.get().installPlugin(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				cleanUploads();
				destroy();
			}

			@Override
			public void onSuccess(Void result) {
				cleanUploads();
				GuiLog.info(I18N.message("plugininstalled"));
				pluginsPanel.refresh();
				destroy();
			}

		});
	}
}
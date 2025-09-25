package com.logicaldoc.gui.frontend.client.workflow.designer;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload a new workflow schema to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.3
 */
public class WorkflowUploader extends Window {

	private Upload uploader;

	private IButton submitButton;

	private VLayout layout = new VLayout();

	private WorkflowDesigner designer;

	public WorkflowUploader(WorkflowDesigner designer) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		this.designer = designer;
		setTitle(I18N.message("uploadworkflow"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(400);
		setHeight(180);
		centerInPage();

		layout.setMembersMargin(2);
		layout.setMargin(2);

		submitButton = new IButton(I18N.message("submit"));
		submitButton.addClickHandler(event -> onSubmit());

		uploader = new Upload(submitButton);
		layout.addMember(uploader);
		layout.addMember(submitButton);
		addItem(layout);
	}

	public void onSubmit() {
		if (uploader.getUploadedFile() == null) {
			SC.warn(I18N.message("filerequired"));
			return;
		}

		WorkflowService.Instance.get().importSchema(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				SC.warn(caught.getMessage());
			}

			@Override
			public void onSuccess(GUIWorkflow result) {
				if (result != null) {
					result.setId(designer.getWorkflow().getId());
					result.setName(designer.getWorkflow().getName());
					result.setLabel(designer.getWorkflow().getLabel());
					designer.redraw(result);
					designer.saveModel();

					// Cleanup the upload folder
					DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							destroy();
						}

						@Override
						public void onSuccess(Void result) {
							destroy();
						}
					});
				}
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
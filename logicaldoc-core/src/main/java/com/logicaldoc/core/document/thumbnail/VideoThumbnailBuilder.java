package com.logicaldoc.core.document.thumbnail;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.bytedeco.javacv.FFmpegFrameGrabber;
import org.bytedeco.javacv.FrameGrabber;
import org.bytedeco.javacv.FrameGrabber.Exception;
import org.bytedeco.javacv.Java2DFrameUtils;
import org.bytedeco.javacv.OpenCVFrameGrabber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;

/**
 * Takes care of generating thumbnails for videos
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2
 */
public class VideoThumbnailBuilder extends AbstractThumbnailBuilder {

	protected static Logger log = LoggerFactory.getLogger(VideoThumbnailBuilder.class);

	@Override
	public synchronized void buildThumbnail(String sid, Document document, String fileVersion, File src, File dest,
			int size, int quality) throws IOException {
		File frameImage = FileUtil.createTempFile("album-", ".png");
		try {
			if (document.getFileName().toLowerCase().endsWith(".mp4"))
				writeMp4Frame(src, frameImage);
			else
				writeVideoFrame(src, frameImage);

			ImageThumbnailBuilder imageTBuilder = new ImageThumbnailBuilder();
			imageTBuilder.buildThumbnail(sid, document, fileVersion, frameImage, dest, size, quality);
		} catch (Exception e) {
			throw new IOException("Error in extracting video frame", e);
		} finally {
			FileUtils.deleteQuietly(frameImage);
		}
	}

	private void writeMp4Frame(File videoFile, File frameFile) throws Exception {
		try (FFmpegFrameGrabber grabber = new FFmpegFrameGrabber(videoFile);) {
			grabber.start();

			/*
			 * Get a frame in the middle of the video
			 */
			int startFrame = grabber.getLengthInVideoFrames() / 2;
			grabber.setVideoFrameNumber(startFrame);

			for (int i = startFrame; i < grabber.getLengthInFrames() && frameFile.length() == 0; i++) {
				try {
					BufferedImage img = Java2DFrameUtils.toBufferedImage(grabber.grab());
					ImageIO.write(img, "png", frameFile);
				} catch (java.lang.Exception e) {
					// Nothing to do
				}
			}
		}
	}

	private void writeVideoFrame(File videoFile, File frameFile) throws Exception {
		try (FrameGrabber grabber = new OpenCVFrameGrabber(videoFile);) {
			grabber.start();

			/*
			 * Try to get a frame after 60 seconds
			 */
			double frameRate = grabber.getFrameRate();
			int fiveSecondsFrame = (int) (60 * frameRate);
			if (fiveSecondsFrame > grabber.getLengthInFrames())
				fiveSecondsFrame = 1;

			for (int i = 0; i < grabber.getLengthInFrames() && frameFile.length() == 0; i++) {
				try {
					if (i < fiveSecondsFrame) {
						grabber.grab();
						continue;
					}

					BufferedImage img = Java2DFrameUtils.toBufferedImage(grabber.grab());
					ImageIO.write(img, "png", frameFile);
				} catch (IOException ioe) {
					// Nothing to do
				}
			}
		}
	}

}
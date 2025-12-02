package ahc.dms.dao.dms.services;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.util.StringUtils;

@Service
public class FileStorageService {

    @Value("${grocify.upload.dir}")
    private String uploadDir;

    public String store(MultipartFile file) throws IOException {

        Path dir = Paths.get(uploadDir);
        Files.createDirectories(dir);

        String filename = System.currentTimeMillis() + "_" +
                StringUtils.cleanPath(file.getOriginalFilename());

        Path dest = dir.resolve(filename);

        try (InputStream in = file.getInputStream()) {
            Files.copy(in, dest, StandardCopyOption.REPLACE_EXISTING);
        }

        return filename; // return filename to store in DB
    }
}

package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.dao._payloads.ApiResponse;
import ahc.dms.dao._payloads.PostDto;
import ahc.dms.dao._payloads.PostResponse;
import ahc.dms.dao.services.FileService;
import ahc.dms.dao.services.PostService;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StreamUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

@RestController
@RequestMapping("/blog")
public class PostController {

    @Autowired
    private PostService postService;
    @Autowired
    private FileService fileService;
    @Value("${project.image}")
    private String path;

    @PostMapping("/user/{userId}/category/{categoryId}/post")
    public ResponseEntity<PostDto> createPost(
            @Valid @RequestBody PostDto postDto,
            @PathVariable("userId") Integer userId,
            @PathVariable("categoryId") Integer categoryId
    ){
        PostDto createdPostDto = postService.createPost(postDto,userId,categoryId);
        return new ResponseEntity<>(createdPostDto, HttpStatus.CREATED);
    }

    @PutMapping("/posts/{postId}")
    public ResponseEntity<PostDto> updatePost(@RequestBody PostDto postDto, @PathVariable("postId") Integer postId) {
        PostDto updatedPost = postService.updatePost(postDto, postId);
        return ResponseEntity.ok(updatedPost);
    }

    @DeleteMapping("/posts/{postId}")
    public ResponseEntity<ApiResponse> deletePost(@PathVariable("postId") Integer postId){
        postService.deletePost(postId);
        return new ResponseEntity<>(new ApiResponse("post deleted", true), HttpStatus.OK);
    }

    @GetMapping("/posts")
    public ResponseEntity<PostResponse> getAllPosts(
            @RequestParam(value = "pageNumber", defaultValue = AppConstants.PAGE_NUMBER, required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", defaultValue = AppConstants.PAGE_SIZE, required = false) Integer pageSize,
            @RequestParam(value = "sortBy", defaultValue = AppConstants.SORT_BY, required = false) String sortBy,
            @RequestParam(value = "sortDir", defaultValue = AppConstants.SORT_DIR, required = false) String sortDir
    ){
        PostResponse postResponse = postService.getAllPosts(pageNumber, pageSize, sortBy, sortDir);
        return new ResponseEntity<>(postResponse, HttpStatus.OK);
    }

    @GetMapping("/posts/{postId}")
    public ResponseEntity<PostDto> getSinglePost(@PathVariable("postId") Integer postId){
        return ResponseEntity.ok(postService.getPostById(postId));
    }

    @GetMapping("/user/{userId}/posts")
    public ResponseEntity<List<PostDto>> getPostByUserId(@PathVariable("userId") Integer userId){
        List<PostDto> posts = postService.getPostsByUser(userId);
        return new ResponseEntity<>(posts, HttpStatus.OK);
    }

    @GetMapping("/category/{categoryId}/posts")
    public ResponseEntity<List<PostDto>> getPostByCategoryId(@PathVariable("categoryId") Integer categoryId){
        List<PostDto> posts = postService.getPostsByCategory(categoryId);
        return new ResponseEntity<>(posts, HttpStatus.OK);
    }

    @GetMapping("/posts/search/{keyword}")
    public ResponseEntity<List<PostDto>> searchPostByTitle(@PathVariable("keyword") String keyword){
        List<PostDto> posts = postService.searchPosts(keyword);
        return new ResponseEntity<>(posts, HttpStatus.OK);
    }

    @PostMapping("/post/image/upload/{postId}")
    public ResponseEntity<PostDto> uploadPostImage(
            @RequestParam("image")MultipartFile image,
            @PathVariable("postId") Integer postId
    ) throws IOException {

        PostDto postDto = postService.getPostById(postId);
        String fileName = fileService.uploadImage(path, image);
        System.out.println(fileName);
        postDto.setImageName(fileName);
        PostDto updatedPost = postService.updatePost(postDto, postId);
        return new ResponseEntity<>(updatedPost, HttpStatus.CREATED);
    }

    @GetMapping(value = "/post/image/{imageName}", produces = MediaType.IMAGE_JPEG_VALUE)
    public void downloadImage(
            @PathVariable("imageName") String imageName,
            HttpServletResponse response
    ) throws IOException {
        InputStream resource = fileService.getResource(path, imageName);
        response.setContentType(MediaType.IMAGE_JPEG_VALUE);
        StreamUtils.copy(resource, response.getOutputStream());
    }

}

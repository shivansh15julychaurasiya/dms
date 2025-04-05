package ahc.dms.dao.services;

import ahc.dms.dao.entities.Category;
import ahc.dms.dao.entities.Post;
import ahc.dms.dao.entities.User;
import ahc.dms.dao._payloads.PostDto;
import ahc.dms.dao._payloads.PostResponse;
import ahc.dms.dao.respositories.CategoryRepository;
import ahc.dms.dao.respositories.PostRepository;
import ahc.dms.dao.respositories.UserRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import jakarta.transaction.Transactional;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class PostService {


    @Autowired
    private PostRepository postRepository;
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private CategoryRepository categoryRepository;
    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public PostDto createPost(PostDto postDto, Integer userId, Integer categoryId) {

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User", "Id", userId));
        Category category = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category", "Id", categoryId));

        Post post = modelMapper.map(postDto, Post.class);
        post.setPostId(null);
        post.setImageName("default.png");
        post.setAddedDate(new Date());
        post.setUser(user);
        post.setCategory(category);
        Post addedPost = postRepository.save(post);

        return modelMapper.map(addedPost, PostDto.class);
    }

    @Transactional
    public PostDto updatePost(PostDto postDto, Integer postId){
        Post post = postRepository.findById(postId)
                .orElseThrow(() -> new ResourceNotFoundException("Post", "Id", postId));
        post.setTitle(postDto.getTitle());
        post.setContent(postDto.getContent());
        post.setImageName(postDto.getImageName());
        Post updatedPost = postRepository.save(post);
        return modelMapper.map(updatedPost, PostDto.class);
    }

    @Transactional
    public void deletePost(Integer postId){
        Post post = postRepository.findById(postId)
                .orElseThrow(() -> new ResourceNotFoundException("Post", "Id", postId));
        postRepository.delete(post);
    }

    public PostDto getPostById(Integer postId) {
        Post post = postRepository.findById(postId)
                .orElseThrow(() -> new ResourceNotFoundException("Post", "Id", postId));
        return modelMapper.map(post, PostDto.class);

    }

    public PostResponse getAllPosts(Integer pageNumber, Integer pageSize, String sortBy, String sortDir){

        Sort sort = (sortDir.equalsIgnoreCase("desc")) ? (Sort.by(sortBy).descending()) : (Sort.by(sortBy).ascending());
        Pageable p = PageRequest.of(pageNumber, pageSize, sort);
        Page<Post> pagePosts = postRepository.findAll(p);
        List<Post> allPosts = pagePosts.getContent();

        //List<Post> posts = postRepository.findAll();
        List<PostDto> postDtos = allPosts.stream()
                .map(post -> modelMapper.map(post, PostDto.class)).collect(Collectors.toList());

        PostResponse postResponse = new PostResponse();
        postResponse.setContent(postDtos);
        postResponse.setPageNumber(pagePosts.getNumber());
        postResponse.setPageSize(pagePosts.getSize());
        postResponse.setTotalElements(pagePosts.getTotalElements());
        postResponse.setTotalPages(pagePosts.getTotalPages());
        postResponse.setLastPage(pagePosts.isLast());

        return postResponse;
    }

    public List<PostDto> getPostsByCategory(Integer categoryId) {
        Category category = categoryRepository.findById(categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category", "Id", categoryId));

        List<Post> posts = postRepository.findByCategory(category);
        return posts.stream().map(post -> modelMapper.map(post, PostDto.class)).collect(Collectors.toList());
    }

    public List<PostDto> getPostsByUser(Integer userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User", " id ", userId));

        List<Post> posts = postRepository.findByUser(user);
        return posts.stream().map(post -> modelMapper.map(post, PostDto.class)).collect(Collectors.toList());
    }

    public List<PostDto> searchPosts(String keyword) {
        List<Post> posts = postRepository.findByTitleContaining(keyword);
        return posts.stream().map(post -> modelMapper.map(post, PostDto.class)).collect(Collectors.toList());
    }

}

package cn.addenda.redissonlock.renewlog;

import org.junit.Test;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class TestCompletableFuture {

  @Test
  public void test1() throws ExecutionException, InterruptedException {
    // 如果supplyAsync直接返回, 得到CompletableFuture<String>
    // 现在经过了thenApply的处理, CompletableFuture<String> 转换为 CompletableFuture<Integer>, CompletableFuture是同一个
    CompletableFuture<Integer> completableFuture = CompletableFuture.supplyAsync(() -> {
      // 先执行supplyAsync方法，得到返回值
      return "hello";
    }).thenApply(value -> {
      // 接收到supplyAsync的返回值“hello”
      if ("hello".equals(value)) {
        return 111;
      } else {
        return 000;
      }
    });

    final Integer integer = completableFuture.get();
    System.out.println("结果：" + integer);
  }

  @Test
  public void test2() throws ExecutionException, InterruptedException {
    CompletableFuture<Integer> completableFuture = CompletableFuture.supplyAsync(() -> {
      // 先执行supplyAsync方法，得到返回值
      return "hello";
    }).thenCompose(value -> CompletableFuture.supplyAsync(() -> {
      // thenCompose方法返回一个新的CompletableFuture
      if ("hello".equals(value)) {
        return 111;
      } else {
        return 000;
      }
    }));

    final Integer integer = completableFuture.get();
    System.out.println("结果：" + integer);
  }

}

package cn.addenda.redissonlock.renewlog;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.redisson.api.RedissonClient;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RedissonLockRenewLogWrapper {

  public static RedissonClient wrap(RedissonClient redissonClient) {
    return wrap(redissonClient, true);
  }

  public static RedissonClient wrap(RedissonClient redissonClient, boolean forceInvokeWrap) {
    InvocationHandler ds = existCommonParams(redissonClient.getClass().getClassLoader()) ?
            new RenewLogInvocationHandler1(redissonClient, forceInvokeWrap) : new RenewLogInvocationHandler2(redissonClient, false);
    return (RedissonClient) Proxy.newProxyInstance(
            redissonClient.getClass().getClassLoader(), redissonClient.getClass().getInterfaces(), ds);
  }

  private static boolean existCommonParams(ClassLoader classLoader) {
    try {
     Class.forName("org.redisson.api.options.CommonParams", false, classLoader);
    } catch (ClassNotFoundException e) {
      return false;
    }
    return true;
  }

}

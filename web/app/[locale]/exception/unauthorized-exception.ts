export default function UnauthorizedException() {
  return new Error(
    'Sorry, accessing this resource requires authentication (It is possible that the situation is either not logged in or the login has expired)',
  );
}
